# tunnerl 

[![Build Status](https://travis-ci.org/surik/tunnerl.svg?branch=master)](https://travis-ci.org/surik/tunnerl)

SOCKS4, SOCKS4a and SOCKS5 protocols implementation in Erlang/OTP.


### Features

 * SOCKS: A protocol for TCP proxy across firewalls: [socks4](https://www.openssh.com/txt/socks4.protocol)
   * connect command only
 * SOCKS 4A: A Simple Extension to SOCKS 4 Protocol: [socks4a](https://www.openssh.com/txt/socks4a.protocol)
 * SOCKS Protocol Version 5: [RFC1928](https://www.ietf.org/rfc/rfc1928.txt) 
   * connect command only
   * Username/Password Authentication for SOCKS V5 [RFC1929](https://tools.ietf.org/rfc/rfc1929.txt)
   * ATYPs: IPv4, IPv6 and domain

### Using

1. Add `tunnerl` to your list of dependencies in rebar.config:

```erlang
{deps, [
    {tunnerl, "1.0.0"}
]}.
```

2. Ensure `tunnerl` is started before your application:

```erlang
{applications, [tunnerl]}.
```

3. Configure it to use custom handler::

```erlang
{tunnerl, [
    {protocols, [socks4, socks5]},
    {handler, myapp_handler},
    {acceptors, 10},
    {ip, {0, 0, 0, 0}},
    {port, 1080}
]}.
```

4. Implement `myapp_handler`:

```erlang
-module(myapp_handler).

%% This simple handler module accepts username authentication and
%% allows user with password "pass" do connect command.
%% Also, is accepts all connections on Socks4 for "root".

-export([auth_methods/0, 
         auth/1, 
         handle_command/1]).

auth_methods() -> [username].

auth(#{username := <<"user">>, 
       password := <<"pass">>}) -> 
    accept;
auth(_) -> rejected.

handle_command(#{protocol := socks4, 
                 command := connect,
                 username := <<"root">>}) ->
    accept;
handle_command(#{protocol := socks5, 
                 command := connect,
                 username := <<"user">>}) ->
    accept;
handle_command(_) -> 
    reject.

```

### Testing

Build:

    $ git clone https://github.com/surik/tunnerl.git
    $ cd tunnerl
    $ rebar3 compile

Run tunnerl with simple predefined configuration with socks4/socks5 and no authentication

    $ rebar3 shell --name socks@127.0.0.1 --config tunnerl.config --apps tunnerl

There is a bunch of common tests which can be running:

    $ rebar3 ct

**Note** that IPv6 tests might not been working on your local machine.
