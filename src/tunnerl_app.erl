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
    AuthMod = application:get_env(tunnerl, auth_module, tunnerl_auth_dummy),
    Opts = [{auth, Auth}, {protocols, Proto}, {auth_module, AuthMod}],
    {ok, _} = ranch:start_listener(tunnerl, NumAcceptors, ranch_tcp, 
                                   [{port, Port}, {ip, IP}], 
                                   tunnerl_socks_protocol, Opts),
    tunnerl_sup:start_link().

stop(_State) ->
    ok.
