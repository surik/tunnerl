-module(tunnerl_socks4).

-export([process/1]).

-include("tunnerl.hrl").

-define(VERSION, 16#04).
-define(RSV, 16#00).
-define(IPV4, 16#01).
-define(IPV6, 16#04).

-define(CMD_CONNECT, 16#01).
-define(CMD_BIND, 16#02).

-define(REP_SUCCESS, 16#5a).
-define(REP_FAILED, 16#5b).
-define(REP_NET_NOTAVAILABLE, 16#5c).
-define(REP_FORBBIDEN, 16#5d).

process(#state{socks4 = false} = _State) ->
    socks4_not_supported;
process(State) ->
    cmd(State).

cmd(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    try
    	{ok, <<CMD>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
        {ok, NewState} = doCmd(CMD, State),
        NewState
    catch 
        Type:Reason ->
            ok = Transport:close(ISocket),
            error_logger:error_msg("socks4 protocol module got error ~p:~p", 
                                   [Type, Reason])
    end.

doCmd(?CMD_CONNECT, #state{transport = Transport, 
                           incoming_socket = ISocket, 
                           handler = Handler} = State) ->
    {ok, <<Port:16, Addr0:4/binary>>} = Transport:recv(ISocket, 6, ?TIMEOUT),
    {ok, User} = get_user(Transport, ISocket),
    {ok, Addr} = get_addr(parse_addr(Addr0), Transport, ISocket),
    Request = #{protocol            => socks4,
                command             => connect,
                username            => User,
                source_ip           => State#state.client_ip,
                source_port         => State#state.client_port,
                destination_address => Addr,
                destination_port    => Port},
    case (catch Handler:handle_command(Request)) of
        accept ->
            do_connect(Addr, Port, Addr0, Port, State#state{username = User});
        _Other ->
            ok = Transport:send(ISocket, <<16#00, ?REP_FAILED, Port:16, Addr0/binary>>),
            error(failed)
    end;

doCmd(Cmd, State) ->
    error_logger:error_msg("Command ~p not implemented yet", [Cmd]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_connect(Addr, Port, BAddr, BPort,
           #state{transport = Transport, 
                              incoming_socket= ISocket} = State) ->
    case tunnerl_socks_protocol:connect(Transport, Addr, Port) of
        {ok, OSocket} ->
            ok = Transport:send(ISocket, <<16#00, ?REP_SUCCESS, BPort:16, BAddr/binary>>),
            {ok, State#state{outgoing_socket = OSocket}};
        _e ->
            ok = Transport:send(ISocket, <<16#00, ?REP_FAILED, BPort:16, BAddr/binary>>),
            error(failed)
    end.

parse_addr(<<0,0,0,_:1/binary>>) -> socks4a;
parse_addr(A) ->
    list_to_tuple(binary_to_list(A)).

get_addr(socks4a, Transport, Socket) ->
    case Transport:recv(Socket, 1, ?TIMEOUT) of
        {ok, Data} when Data =/= <<0>> -> 
            {ok, Addr} = recv_till_null(Transport, Socket, Data),
            {ok, binary_to_list(Addr)};
        _ -> {ok, ""}
    end;
get_addr(Addr, _Transport, _Socket) -> {ok, Addr}.

get_user(Transport, Socket) ->
    case Transport:recv(Socket, 1, ?TIMEOUT) of
        {ok, Data} when Data =/= <<0>> -> 
            recv_till_null(Transport, Socket, Data);
        _ -> {ok, ""}
    end.

recv_till_null(Transport, Socket, User) ->
    case Transport:recv(Socket, 1, ?TIMEOUT) of
        {ok, <<Data>>} when Data =/= 0 -> 
            recv_till_null(Transport, Socket, <<User/binary, Data>>);
        {ok, <<0>>} -> {ok, User};
        _ -> {ok, ""}
    end.
