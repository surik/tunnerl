-module(curl).

-export([request/3, request/5]).

request(Proto, Family, Link) when is_atom(Proto) -> request(atom_to_list(Proto), Family, Link);
request(Proto, Family, Link) ->
    Port = integer_to_list(application:get_env(tunnerl, port, 1080)),
    Cmd = "curl " ++ family(Family) ++ " --" ++ Proto ++ " 127.0.0.1:" ++ Port ++ " -I " ++ Link,
    cmd(Cmd).

request(Proto, Family, Link, User, Pass) when is_atom(Proto) -> request(atom_to_list(Proto), Family, Link, User, Pass);
request(Proto, Family, Link, User, Pass) ->
    Port = integer_to_list(application:get_env(tunnerl, port, 1080)),
    Cmd = "curl " ++ family(Family) ++  " --" ++ Proto ++ " " ++ User ++ ":" ++ Pass ++ "@127.0.0.1:" ++ Port ++ " -I " ++ Link,
    cmd(Cmd).

family(inet) -> "-4";
family(inet6) -> "-6";
family(_) -> "-4".

cmd(Cmd) ->
    Result = os:cmd(Cmd),
    % just check that host answers by HTTP
    string:str(Result, "HTTP/1.1") > 0. 
