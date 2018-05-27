-module(test_handler).

-behavior(tunnerl_gen_handler).

-export([auth_methods/0, 
         auth/1, 
         handle_command/1]).

auth_methods() -> 
    application:get_env(tunnerl, auth, []).

auth(#{username := <<"user">>, 
       password := <<"pass">>}) -> 
    accept;
auth(#{username := <<"throw2">>, 
       password := <<"pass">>}) -> 
    accept;
auth(#{username := <<"throw">>}) -> 
    throw(error);
auth(_) -> rejected.

handle_command(#{protocol := _socks4and5, 
                 command := connect,
                 username := <<"user">>}) ->
    accept;
handle_command(#{protocol := socks4, 
                 command := connect,
                 username := <<"throw">>}) ->
    throw(error);
handle_command(#{protocol := _socks4and5, 
                 command := connect,
                 username := <<"throw2">>}) ->
    throw(error);
handle_command(_) -> 
    reject.
