-module(tunnerl).

-compile([export_all]).

start() ->
    {ok, _} = application:ensure_all_started(tunnerl).
    

stop() ->
    ok = application:stop(tunnerl).
