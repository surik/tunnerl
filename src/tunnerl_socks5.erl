-module(tunnerl_socks5).

-export([process/1]).

-include("tunnerl.hrl").

-define(VERSION, 16#05).
-define(RSV, 16#00).
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(CMD_CONNECT, 16#01).
-define(CMD_BIND, 16#02).
-define(CMD_UDP_ASSOCIATE,  16#03).

-define(AUTH_NOAUTH, 16#00).
-define(AUTH_GSSAPI, 16#01).
-define(AUTH_USERNAME, 16#02).
-define(AUTH_UNDEF, 16#FF).

-define(REP_SUCCESS, 16#00).
-define(REP_SERVER_ERROR, 16#01).
-define(REP_NOT_ALLOWED, 16#02).
-define(REP_NET_UNREACHABLE, 16#03).
-define(REP_HOST_UNREACHABLE, 16#04).
-define(REP_CONN_REFUSED, 16#05).
-define(REP_TTL_EXPIRES, 16#06).
-define(REP_CMD_NOTSUPPORTED, 16#07).
-define(REP_ATYP_NOSUPPORTED, 16#08).
-define(REP_UNDEF, 16#FF).

process(#state{socks5 = Socks5} = _State) when Socks5 == false ->
    socks5_not_supported;
process(#state{transport = Transport, socks5 = Socks5} = State) 
  when Socks5 == true ->
    try auth(State)
    catch 
        Type:Reason ->
            Transport:close(State#state.incoming_socket),
            error_logger:error_msg("socks5 protocol module got error ~p:~p", [Type, Reason])
    end.

auth(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    {ok, <<NMethods>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
    {ok, Data} = Transport:recv(ISocket, NMethods, ?TIMEOUT),
    doAuth(Data, State).

doAuth(Data, #state{handler = Handler} = State) ->
    OfferAuthMethods = [cast_method(Method) || Method <- binary_to_list(Data)],
    Methods = lists:filter(fun(Method) -> 
                               lists:member(Method, OfferAuthMethods) 
                           end, Handler:auth_methods()) ++ [undefined],
    doAuth_by_method(Methods, State).

doAuth_by_method([noauth = _Method | _], 
                 #state{transport = Transport, 
                        incoming_socket = ISocket} = State) ->
    % TODO: inform handler?
    Transport:send(ISocket, <<?VERSION, ?AUTH_NOAUTH>>),
    cmd(State);

doAuth_by_method([username = Method | _], 
                 #state{client_ip = CAddr, 
                        client_port = CPort,
                        transport = Transport,
                        incoming_socket = ISocket,
                        handler = Handler} = State) ->
    Transport:send(ISocket, <<?VERSION, ?AUTH_USERNAME>>),
    {ok, <<Version, ULen>>} = Transport:recv(ISocket, 2, ?TIMEOUT),
    {ok, User} = Transport:recv(ISocket, ULen, ?TIMEOUT),
    {ok, <<PLen>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
    {ok, Password} = Transport:recv(ISocket, PLen, ?TIMEOUT),
    Request = #{protocol    => socks5,
                command     => auth,
                method      => Method,
                username    => User,
                password    => Password,
                source_ip   => CAddr,
                source_port => CPort},
    case (catch Handler:auth(Request)) of
        accept -> 
            Transport:send(ISocket, <<Version, ?REP_SUCCESS>>),
            cmd(State#state{username = User});
        _ -> 
            Transport:send(ISocket, <<Version, ?REP_SERVER_ERROR>>),
            error(no_auth)
    end;

doAuth_by_method(_Methods,
                 #state{transport = Transport,
                        incoming_socket = ISocket} = _State) ->
    Transport:send(ISocket, <<?VERSION, ?AUTH_UNDEF>>),
    throw(auth_not_supported).

cmd(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    try
    	{ok, <<?VERSION, CMD, ?RSV, ATYP>>} = Transport:recv(ISocket, 4, ?TIMEOUT),
        {ok, NewState} = doCmd(CMD, ATYP, State),
        NewState
    catch 
        _:Reason ->
            ok = Transport:close(ISocket),
            error_logger:error_msg("~p:~p command error ~p", 
                                   [tunnerl_socks_protocol:pretty_address(State#state.client_ip), 
                                    State#state.client_port, Reason])
    end.

doCmd(?CMD_CONNECT, ATYP, #state{transport = Transport, 
                                 username = User,
                                 incoming_socket = ISocket,
                                 handler = Handler} = State) ->
    {ok, Data} = get_address_port(ATYP, Transport, ISocket),
    {Addr, Port} = parse_addr_port(ATYP, Data),
    Request = #{protocol            => socks5,
                command             => connect,
                username            => User,
                source_ip           => State#state.client_ip,
                source_port         => State#state.client_port,
                destination_address => Addr,
                destination_port    => Port},
    {ok, {BAddr, BPort}} = inet:sockname(ISocket),
    BAddr2 = list_to_binary(tuple_to_list(BAddr)),
    case (catch Handler:handle_command(Request)) of
        accept ->
            do_connect(Addr, Port, BAddr2, BPort, State#state{username = User});
        reject->
            ok = Transport:send(ISocket, <<?VERSION, ?REP_NOT_ALLOWED, ?RSV, ?IPV4, BAddr2/binary, BPort:16>>),
            error(no_allowed);
        _ ->
            ok = Transport:send(ISocket, <<?VERSION, ?REP_SERVER_ERROR, ?RSV, ?IPV4, BAddr2/binary, BPort:16>>),
            error(server_error)
    end;

doCmd(Cmd, _, State) ->
    error_logger:error_msg("Command ~p not implemented yet", [Cmd]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
cast_method(?AUTH_NOAUTH)   -> noauth;
cast_method(?AUTH_GSSAPI)   -> gssapi;
cast_method(?AUTH_USERNAME) -> username;
cast_method(?AUTH_UNDEF)    -> undefined;
cast_method(Number)
  when is_integer(Number) -> Number.

do_connect(Addr, Port, SAddr, SPort, 
           #state{transport = Transport, 
                  incoming_socket= ISocket} = State) ->
    case tunnerl_socks_protocol:connect(Transport, Addr, Port) of
        {ok, OSocket} ->
            ok = Transport:send(ISocket, <<?VERSION, ?REP_SUCCESS, ?RSV, ?IPV4, SAddr/binary, SPort:16>>),
            {ok, State#state{outgoing_socket = OSocket}};
        {error, Error0} ->
            Error = connect_error_map(Error0),
            ok = Transport:send(ISocket, <<?VERSION, Error, ?RSV, ?IPV4, SAddr/binary, SPort:16>>),
            error(Error0);
        _e ->
            ok = Transport:send(ISocket, <<16#00, ?REP_SERVER_ERROR, SPort:16, SAddr/binary>>),
            error(server_error)
    end.

connect_error_map(ehostunreach) -> ?REP_HOST_UNREACHABLE;
connect_error_map(enetunreach)  -> ?REP_NET_UNREACHABLE;
connect_error_map(erefused)     -> ?REP_CONN_REFUSED;
connect_error_map(_)            -> ?REP_SERVER_ERROR.

get_address_port(ATYP, Transport, Socket) ->
    case ATYP of
        ?IPV4 -> Transport:recv(Socket, 6, ?TIMEOUT);
        ?IPV6 -> Transport:recv(Socket, 18, ?TIMEOUT);
        ?DOMAIN ->
            {ok, <<DLen>>} = Transport:recv(Socket, 1, ?TIMEOUT),
            {ok, AddrPort} = Transport:recv(Socket, DLen+2, ?TIMEOUT),
            {ok, <<DLen, AddrPort/binary>>};
        true -> throw(unknown_atyp)
    end.

parse_addr_port(?IPV4, <<Addr:4/binary, Port:16>>) ->
    {list_to_tuple(binary_to_list(Addr)), Port};
parse_addr_port(?IPV6, <<Addr:16/binary, Port:16>>) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>> = Addr,
    {{A,B,C,D,E,F,G,H}, Port};
parse_addr_port(?DOMAIN, <<Len, Addr:Len/binary, Port:16>>) ->
    {binary_to_list(Addr), Port}.
