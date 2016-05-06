-module(tunnerl_auth_dummy).

-export([auth/4]).

auth(_Proto, _User, _Password, _Options) -> 
    ok.
