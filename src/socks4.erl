-module(socks4).

-export([process/1]).

-include("socks.hrl").

-define(VERSION, 16#04). % only SOCKS5
-define(RSV, 16#00).
-define(IPV4, 16#01).
-define(IPV6, 16#04).

-define(CMD_CONNECT, 16#01).
-define(CMD_BIND, 16#02).

-define(REP_SUCCESS, 16#5a).
-define(REP_FAILED, 16#5b).
-define(REP_FORBBIDEN, 16#5c).
-define(REP_NET_NOTAVAILABLE, 16#5d).

process(#state{socks4 = false} = _State) ->
    lager:debug("SOCKS4 unsupported."),
    socks4_not_supported;
process(State) ->
    cmd(State).

cmd(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    try
    	{ok, <<CMD>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
        {ok, NewState} = doCmd(CMD, State),
        NewState
    catch 
        _:no_auth ->
            ok = Transport:close(ISocket);
        _:Reason ->
            ok = Transport:close(ISocket),
            lager:error("~p:~p command error ~p", [socks_protocol:pretty_address(State#state.client_ip), 
                                                   State#state.client_port, Reason])
    end.

doCmd(?CMD_CONNECT, #state{transport = Transport, incoming_socket = ISocket, auth_mod = AuthMod} = State) ->
    {ok, <<Port:16, A:4/binary>>} = Transport:recv(ISocket, 6, ?TIMEOUT),
    Addr = list_to_tuple(binary_to_list(A)),
    {ok, User} = get_user(Transport, ISocket),
    AuthOpts = [{client_ip, State#state.client_ip}, 
                {client_port, State#state.client_port}],
    case AuthMod:auth(socks4, User, "", AuthOpts) of
        ok ->
            {ok, OSocket} = socks_protocol:connect(Transport, Addr, Port),
            lager:info("~p:~p connected to ~p:~p", [socks_protocol:pretty_address(State#state.client_ip), 
                                                    State#state.client_port,
                                                    socks_protocol:pretty_address(Addr), Port]),
            {ok, {BAddr, BPort}} = inet:sockname(ISocket),
            BAddr2 = list_to_binary(tuple_to_list(BAddr)),
            ok = Transport:send(ISocket, <<16#00, ?REP_SUCCESS, BPort:16, BAddr2/binary>>),
            {ok, State#state{outgoing_socket = OSocket}};
        _ ->
            ok = Transport:send(ISocket, <<16#00, ?REP_NET_NOTAVAILABLE, Port:16, A/binary>>),
            lager:info("~p:~p Authorization for ~p failed", [socks_protocol:pretty_address(State#state.client_ip),
                                                             State#state.client_port, User]),
            error(no_auth)
    end;

doCmd(Cmd, State) ->
    lager:error("Command ~p not implemented yet", [Cmd]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_user(Transport, Socket) ->
    case Transport:recv(Socket, 1, ?TIMEOUT) of
        {ok, Data} when Data =/= <<0>> -> get_user(Transport, Socket, Data);
        _ -> {ok, ""}
    end.

get_user(Transport, Socket, User) ->
    case Transport:recv(Socket, 1, ?TIMEOUT) of
        {ok, <<Data>>} when Data =/= 0 -> get_user(Transport, Socket, <<User/binary, Data>>);
        {ok, <<0>>} -> {ok, User};
        _ -> {ok, ""}
    end.
