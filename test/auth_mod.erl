-module(auth_mod).

-export([auth/4]).

auth(socks4, <<"user">>, _Password, _Options) -> ok;
auth(socks5, <<"user">>, <<"pass">>, _Options) -> ok;
auth(_Proto, _User, _Password, _Options) -> error.
