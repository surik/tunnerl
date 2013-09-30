-module(tunnerl).

-compile([export_all]).

start() ->
    ok = application:start(lager),
    ok = application:start(ranch),
    ok = application:start(tunnerl).
    

stop() ->
    ok = application:stop(tunnerl).
