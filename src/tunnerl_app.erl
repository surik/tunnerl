-module(tunnerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("socks5.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = application:get_env(tunnerl, port, 1080),
    Auth = application:get_env(tunnerl, auth, [?AUTH_NOAUTH]),
    Opts = [{auth, Auth}],
    {ok, _} = ranch:start_listener(tunnerl, 10, ranch_tcp, [{port, Port}], socks5_protocol, Opts),
    tunnerl_sup:start_link().

stop(_State) ->
    ok.
