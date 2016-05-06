-module(socks5).

-export([process/1]).

-include("socks.hrl").

-define(VERSION, 16#05).
-define(RSV, 16#00).
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(CMD_CONNECT, 16#01).
-define(CMD_BIND, 16#02).
-define(CMD_UDP_ASSOCIATE,  16#03).

-define(AUTH_NOAUTH, 16#00).
-define(AUTH_GSAPI, 16#01).
-define(AUTH_USERNAME, 16#02).
-define(AUTH_UNDEF, 16#FF).

-define(REP_SUCCESS, 16#00).
-define(REP_SERVER_ERROR, 16#01).
-define(REP_FORBBIDEN, 16#02).
-define(REP_NET_NOTAVAILABLE, 16#03).
-define(REP_HOST_NOTAVAILABLE, 16#04).
-define(REP_FAILURE, 16#05).
-define(REP_TTL_EXPIRES, 16#06).
-define(REP_CMD_NOTSUPPORTED, 16#07).
-define(REP_ATYP_NOSUPPORTED, 16#08).
-define(REP_UNDEF, 16#FF).

process(#state{socks5 = Socks5} = _State) when Socks5 == false ->
    lager:error("SOCKS5 unsupported."),
    socks5_not_supported;
process(#state{socks5 = Socks5} = State) when Socks5 == true ->
    try auth(State)
    catch 
        _:Reason ->
            Transport = State#state.transport,
            Transport:close(State#state.incoming_socket),
            lager:error("Auth error ~p", [Reason])
    end.

auth(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    {ok, <<NMethods>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
    {ok, Data} = Transport:recv(ISocket, NMethods, ?TIMEOUT),
    doAuth(Data, State).

doAuth(Data, #state{auth_methods = AuthMethods, auth_mod = AuthMod, 
                    transport = Transport, incoming_socket = ISocket} = State) ->
    OfferAuthMethods = binary_to_list(Data),
    CAddr = State#state.client_ip,
    CPort = State#state.client_port,
    lager:info("~p:~p offers authentication methods: ~p", [socks_protocol:pretty_address(CAddr),
                                                           CPort, OfferAuthMethods]),
    [Method | _] = lists:filter(fun(E) -> lists:member(E, OfferAuthMethods) end, AuthMethods),
    % only no authentication support now
    case Method of
        ?AUTH_NOAUTH -> 
            Transport:send(ISocket, <<?VERSION, Method>>),
            lager:info("~p:~p Authorized with ~p type", [socks_protocol:pretty_address(CAddr), CPort, Method]),
            cmd(State);
        ?AUTH_USERNAME -> 
            Transport:send(ISocket, <<?VERSION, Method>>),
            {ok, <<Version>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
            {ok, <<ULen>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
            {ok, User} = Transport:recv(ISocket, ULen, ?TIMEOUT),
            {ok, <<PLen>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
            {ok, Password} = Transport:recv(ISocket, PLen, ?TIMEOUT),
            AuthOpts = [{client_ip, CAddr}, 
                        {client_port, CPort}],
            case AuthMod:auth(socks5, User, Password, AuthOpts) of
                ok -> 
                    lager:info("~p:~p Authorized with ~p type", [socks_protocol:pretty_address(CAddr), CPort, Method]),
                    Transport:send(ISocket, <<Version, ?REP_SUCCESS>>),
                    cmd(State);
                _ -> 
                    Transport:send(ISocket, <<Version, ?REP_SERVER_ERROR>>),
                    error(no_auth)
            end;
        _ ->
            Transport:send(ISocket, <<?VERSION, ?AUTH_UNDEF>>),
            lager:error("~p:~p Authorization method (~p) not supported", [socks_protocol:pretty_address(CAddr),
                                                                          CPort, OfferAuthMethods]),
            throw(auth_not_supported)
    end.

cmd(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    try
    	{ok, <<?VERSION, CMD, ?RSV, ATYP>>} = Transport:recv(ISocket, 4, ?TIMEOUT),
        {ok, NewState} = doCmd(CMD, ATYP, State),
        NewState
    catch 
        _:Reason ->
            ok = Transport:close(ISocket),
            lager:error("~p:~p command error ~p", [socks_protocol:pretty_address(State#state.client_ip), 
                                                   State#state.client_port, Reason])
    end.

doCmd(?CMD_CONNECT, ATYP, #state{transport = Transport, incoming_socket = ISocket} = State) ->
    {ok, Data} = get_address_port(ATYP, Transport, ISocket),
    {Addr, Port} = parse_addr_port(ATYP, Data),
    {ok, OSocket} = socks_protocol:connect(Transport, Addr, Port),
    lager:info("~p:~p connected to ~p:~p", [socks_protocol:pretty_address(State#state.client_ip), 
                                            State#state.client_port,
                                            socks_protocol:pretty_address(Addr), Port]),
    {ok, {BAddr, BPort}} = inet:sockname(ISocket),
    BAddr2 = list_to_binary(tuple_to_list(BAddr)),
    ok = Transport:send(ISocket, <<?VERSION, ?REP_SUCCESS, ?RSV, ?IPV4, BAddr2/binary, BPort:16>>),
    {ok, State#state{outgoing_socket = OSocket}};

doCmd(Cmd, _, State) ->
    lager:error("Command ~p not implemented yet", [Cmd]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
    {list_to_tuple(binary_to_list(Addr)), Port};
parse_addr_port(?DOMAIN, <<Len, Addr:Len/binary, Port:16>>) ->
    {binary_to_list(Addr), Port}.
