-module(tunnerl_socks_protocol).

-behavior(ranch_protocol).

%% ranch_protocol callbacks
-export([start_link/3,
         init/3]).

-export([connect/3,
         pretty_address/1]).
-export([loop/1]).

-include("tunnerl.hrl").

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, Opts) ->
    Handler = proplists:get_value(handler, Opts, tunnerl_handler_dummy),
    Protocols = proplists:get_value(protocols, Opts),
    {ok, Socket} = ranch:handshake(Ref),
    {ok, {Addr, Port}} = inet:peername(Socket),
    State = #state{socks4 = lists:member(socks4, Protocols),
                   socks5 = lists:member(socks5, Protocols),
                   handler = Handler,
                   transport = Transport,
                   client_ip = Addr,
                   client_port = Port,
                   incoming_socket = Socket},
    {ok, <<Version>>} = Transport:recv(Socket, 1, ?TIMEOUT),
    case Version of
        ?VERSION5 -> loop(tunnerl_socks5:process(State));
        ?VERSION4 -> loop(tunnerl_socks4:process(State));
        _ ->
            Transport:close(Socket)
    end.

loop(#state{transport = Transport,
            incoming_socket = ISocket,
            outgoing_socket = OSocket} = State) ->
    inet:setopts(ISocket, [{active, once}]),
    OSocket /= undefined andalso inet:setopts(OSocket, [{active, once}]),
    {OK, Closed, Error, _Passive} = Transport:messages(),
    receive
        {OK, ISocket, Data} ->
            Transport:send(OSocket, Data),
            ?MODULE:loop(State);
        {OK, OSocket, Data} ->
            Transport:send(ISocket, Data),
            ?MODULE:loop(State);
        {Closed, ISocket} ->
            % TODO: inform handler
            Transport:close(OSocket);
        {Closed, OSocket} ->
            % TODO: inform handler
            Transport:close(ISocket);
        {Error, _ISocket, _Reason} ->
            % TODO: inform handler
            Transport:close(OSocket);
        {Error, _OSocket, _Reason} ->
            % TODO: inform handler
            Transport:close(ISocket);
        Other ->
            io:format("~p~n", [Other])
    end;
loop(_) -> ok.

connect(Transport, Addr, Port) ->
    connect(Transport, Addr, Port, 2).

connect(Transport, Addr, Port, 0) ->
    Family = inet_family(Addr),
    Transport:connect(Addr, Port, Family);
connect(Transport, Addr, Port, Ret) ->
    Family = inet_family(Addr),
    case Transport:connect(Addr, Port, Family) of
        {ok, OSocket} -> {ok, OSocket};
        {error, _} -> connect(Transport, Addr, Port, Ret-1)
    end.

inet_family({_,_,_,_}) -> [inet];
inet_family(IP) when is_tuple(IP) -> [inet6];
inet_family(_) -> [].

pretty_address(Addr) when is_tuple(Addr) ->
    inet_parse:ntoa(Addr);
pretty_address(Addr) ->
    Addr.

%%%===================================================================
%%% Internal functions
%%%===================================================================
