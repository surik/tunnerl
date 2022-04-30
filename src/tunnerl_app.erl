-module(tunnerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    IP = application:get_env(tunnerl, ip, {0, 0, 0, 0}),
    Port = application:get_env(tunnerl, port, 1080),
    NumAcceptors = application:get_env(tunnerl, acceptors, 10),
    Auth = application:get_env(tunnerl, auth, [0]),
    Proto = application:get_env(tunnerl, protocols, 1080),
    Handler = application:get_env(tunnerl, handler, tunnerl_handler_dummy),

    TransportOpts = #{socket_opts => [{port, Port}, {ip, IP}], num_acceptors => NumAcceptors},
    Opts = [{auth, Auth},
            {protocols, Proto},
            {handler, Handler}],

    {ok, _} = ranch:start_listener(tunnerl, ranch_tcp,
                                   TransportOpts,
                                   tunnerl_socks_protocol, Opts),

    tunnerl_sup:start_link().

stop(_State) ->
    ok.
