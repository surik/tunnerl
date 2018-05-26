-module(tunnerl_handler_dummy).

-behavior(tunnerl_gen_handler).

-export([auth_methods/0, auth/1, handle_command/1]).

auth_methods() -> [noauth].

auth(_Request) ->
    accept.

handle_command(_Request) ->
    accept.
