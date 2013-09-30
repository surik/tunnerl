-module(socks5_protocol).

-behavior(ranch_protocol).

%% ranch_protocol callbacks
-export([start_link/4, init/4]).

-include("socks5.hrl").

-define(TIMEOUT, timer:seconds(5)).

-record(state, {
    incoming_socket :: gen_tcp:socket(),
    outgoing_socket :: gen_tcp:socket(),
    client_ip :: inet:ip_address(),
    client_port :: inet:port_number(),
    auth_methods :: list(),
    transport :: module()
}).


start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
    AuthMethods = proplists:get_value(auth, Opts),
	ok = ranch:accept_ack(Ref),
    {ok, {Addr, Port}} = inet:peername(Socket),
    State = #state{auth_methods = AuthMethods, 
                   transport = Transport, 
                   client_ip = Addr,
                   client_port = Port,
                   incoming_socket = Socket},
    try auth(State)
    catch 
        _:Reason ->
			Transport:close(Socket),
            lager:error("Auth error ~p", [Reason])
    end.

auth(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    {ok, <<?VERSION, NMethods>>} = Transport:recv(ISocket, 2, ?TIMEOUT),
    {ok, Data} = Transport:recv(ISocket, NMethods, ?TIMEOUT),
    doAuth(Data, State).

doAuth(Data, #state{auth_methods = AuthMethods, transport = Transport, incoming_socket = ISocket} = State) ->
    OfferAuthMethods = binary_to_list(Data),
    Addr = State#state.client_ip,
    Port = State#state.client_port,
    lager:info("~p:~p offers authentication methods: ~p", [pretty_address(Addr), Port, OfferAuthMethods]),
    [Method | _] = lists:filtermap(fun(E) -> lists:member(E, OfferAuthMethods) end, AuthMethods),
    % only no authentication support now
    case Method of
        ?AUTH_NOAUTH -> 
            Transport:send(ISocket, <<?VERSION, Method>>),
            lager:info("~p:~p Authorized with ~p type", [pretty_address(Addr), Port, Method]),
            cmd(State);
        _ ->
            Transport:send(ISocket, <<?VERSION, ?AUTH_UNDEF>>),
            lager:error("~p:~p Authorization method (~p) not supported", [pretty_address(Addr), Port, OfferAuthMethods]),
            throw(auth_not_supported)
    end.

cmd(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    try
    	{ok, <<?VERSION, CMD, ?RSV, ATYP>>} = Transport:recv(ISocket, 4, ?TIMEOUT),
        {ok, NewState} = doCmd(CMD, ATYP, State),
        loop(NewState)
    catch 
        _:Reason ->
			ok = Transport:close(ISocket),
            lager:error("~p:~p command error ~p", [pretty_address(State#state.client_ip), 
                                                   State#state.client_port, Reason])
    end.

doCmd(?CMD_CONNECT, ATYP, #state{transport = Transport, incoming_socket = ISocket} = State) ->
    {ok, Data} = get_address_port(ATYP, Transport, ISocket),
    {Addr, Port} = parse_addr_port(ATYP, Data),
    {ok, OSocket} = connect(Transport, Addr, Port),
    lager:info("~p:~p connected to ~p:~p", [pretty_address(State#state.client_ip), State#state.client_port,
                                            pretty_address(Addr), Port]),
    {ok, {BAddr, BPort}} = inet:sockname(ISocket),
    BAddr2 = list_to_binary(tuple_to_list(BAddr)),
    ok = Transport:send(ISocket, <<?VERSION, ?REP_SUCCESS, ?RSV, ?IPV4, BAddr2/binary, BPort:16>>),
    {ok, State#state{outgoing_socket = OSocket}};

doCmd(Cmd, _, State) ->
    lager:error("Command ~p not implemented yet", [Cmd]),
    {ok, State}.

loop(#state{transport = Transport, incoming_socket = ISocket, outgoing_socket = OSocket} = State) ->
    inet:setopts(ISocket, [{active, once}]),
    inet:setopts(OSocket, [{active, once}]),
    {OK, Closed, Error} = Transport:messages(),
    receive
        {OK, ISocket, Data} ->
            Transport:send(OSocket, Data),
            loop(State);
        {OK, OSocket, Data} ->
            Transport:send(ISocket, Data),
            loop(State);
        {Closed, ISocket} ->
            lager:info("~p:~p closed!", [pretty_address(State#state.client_ip), State#state.client_port]),
            Transport:close(OSocket);
        {Closed, OSocket} ->
            lager:info("~p:~p closed!", [pretty_address(State#state.client_ip), State#state.client_port]),
            Transport:close(ISocket);
        {Error, ISocket, Reason} ->
            lager:error("incoming socket: ~p", [Reason]),
            lager:info("~p:~p closed!", [pretty_address(State#state.client_ip), State#state.client_port]),
            Transport:close(OSocket);
        {Error, OSocket, Reason} ->
            lager:error("outgoing socket: ~p", [Reason]),
            lager:info("~p:~p closed!", [pretty_address(State#state.client_ip), State#state.client_port]),
            Transport:close(ISocket)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
connect(Transport, Addr, Port) ->
    connect(Transport, Addr, Port, 2).

connect(Transport, Addr, Port, 0) ->
    Transport:connect(Addr, Port, []);
connect(Transport, Addr, Port, Ret) ->
    case Transport:connect(Addr, Port, []) of
        {ok, OSocket} -> {ok, OSocket};
        {error, _} -> connect(Transport, Addr, Port, Ret-1)
    end.

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

pretty_address(Addr) when is_tuple(Addr) ->
    inet_parse:ntoa(Addr);
pretty_address(Addr) ->
    Addr.

parse_addr_port(?IPV4, <<Addr:4/binary, Port:16>>) ->
    {list_to_tuple(binary_to_list(Addr)), Port};
parse_addr_port(?IPV6, <<Addr:16/binary, Port:16>>) ->
    {list_to_tuple(binary_to_list(Addr)), Port};
parse_addr_port(?DOMAIN, <<Len, Addr:Len/binary, Port:16>>) ->
    {binary_to_list(Addr), Port}.
