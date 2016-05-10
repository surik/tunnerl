-module(curl).

-export([request/2, request/4]).

request(Proto, Link) when is_atom(Proto) -> request(atom_to_list(Proto), Link);
request(Proto, Link) ->
    Port = integer_to_list(application:get_env(tunnerl, port, 1080)),
    Cmd = "curl --" ++ Proto ++ " localhost:" ++ Port ++ " -I " ++ Link,
    cmd(Cmd).

request(Proto, Link, User, Pass) when is_atom(Proto) -> request(atom_to_list(Proto), Link, User, Pass);
request(Proto, Link, User, Pass) ->
    Port = integer_to_list(application:get_env(tunnerl, port, 1080)),
    Cmd = "curl --" ++ Proto ++ " " ++ User ++ ":" ++ Pass ++ "@localhost:" ++ Port ++ " -I " ++ Link,
    cmd(Cmd).

cmd(Cmd) ->
    Result = os:cmd(Cmd),
    % just check that host answers by HTTP
    string:str(Result, "HTTP/1.1") > 0. 
