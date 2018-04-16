-module(tunnerl).

-export([start/0, stop/0]).

start() ->
    {ok, _} = application:ensure_all_started(tunnerl).
    

stop() ->
    ok = application:stop(tunnerl).
