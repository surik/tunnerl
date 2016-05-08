-module(socks_protocol).

-behavior(ranch_protocol).

%% ranch_protocol callbacks
-export([start_link/4, 
         init/4]).

-export([connect/3, 
         pretty_address/1]).
-export([loop/1]).

-include("socks.hrl").

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
    AuthMethods = proplists:get_value(auth, Opts),
    AuthMod = proplists:get_value(auth_module, Opts, tunnerl_auth_dummy),
    Protocols = proplists:get_value(protocols, Opts),
    ok = ranch:accept_ack(Ref),
    {ok, {Addr, Port}} = inet:peername(Socket),
    State = #state{auth_methods = AuthMethods, 
                   socks4 = lists:member(socks4, Protocols),
                   socks5 = lists:member(socks5, Protocols),
                   auth_mod = AuthMod,
                   transport = Transport, 
                   client_ip = Addr,
                   client_port = Port,
                   incoming_socket = Socket},
    {ok, <<Version>>} = Transport:recv(Socket, 1, ?TIMEOUT),
    case Version of
        ?VERSION5 -> loop(socks5:process(State));
        ?VERSION4 -> loop(socks4:process(State));
        _ -> 
            Transport:close(Socket),
            lager:debug("Unsupported SOCKS version ~p", [Version])
    end.

loop(#state{transport = Transport, incoming_socket = ISocket, outgoing_socket = OSocket} = State) ->
    inet:setopts(ISocket, [{active, once}]),
    inet:setopts(OSocket, [{active, once}]),
    {OK, Closed, Error} = Transport:messages(),
    receive
        {OK, ISocket, Data} ->
            Transport:send(OSocket, Data),
            ?MODULE:loop(State);
        {OK, OSocket, Data} ->
            Transport:send(ISocket, Data),
            ?MODULE:loop(State);
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
    end;
loop(_) -> ok.

connect(Transport, Addr, Port) ->
    connect(Transport, Addr, Port, 2).

connect(Transport, Addr, Port, 0) ->
    Transport:connect(Addr, Port, []);
connect(Transport, Addr, Port, Ret) ->
    case Transport:connect(Addr, Port, []) of
        {ok, OSocket} -> {ok, OSocket};
        {error, _} -> connect(Transport, Addr, Port, Ret-1)
    end.

pretty_address(Addr) when is_tuple(Addr) ->
    inet_parse:ntoa(Addr);
pretty_address(Addr) ->
    Addr.

%%%===================================================================
%%% Internal functions
%%%===================================================================
