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
auth(_) -> rejected.

handle_command(#{protocol := socks4, 
                 command := connect,
                 username := <<"user">>}) ->
    accept;
handle_command(#{protocol := socks5, 
                 command := connect,
                 username := <<"user">>}) ->
    accept;
handle_command(_) -> 
    reject.
