# tunnerl 

[![Build Status](https://travis-ci.org/surik/tunnerl.svg?branch=master)](https://travis-ci.org/surik/tunnerl)

SOCKS4, SOCKS4a and SOCKS5 protocols implementation in Erlang/OTP.


### Features

 * SOCKS v4 and SOCKS v4A 
   * connect command only

 * SOCKS v5 
   * only username authorization 
   * connect command only
   * IPv4 and IPv6

### Using

1. Add `tunnerl` to your list of dependencies in rebar.config:

```erlang
{deps, [
    {tunnerl, "0.4.0"}
]}.
```

2. Ensure `tunnerl` is started before your application:

```erlang
{applications, [tunnerl]}.
```

3. Configure it to use custom authorization handler::

```erlang
{tunnerl, [
    {protocols, [socks4, socks5]},
    {auth, [16#02]}, % shows that only username authorization can be accepted
    {auth_module, myapp_auth_handler},
    {acceptors, 10},
    {ip, {0, 0, 0, 0},
    {port, 1080}
]}.
```

4. Implement `myapp_auth_handler`:

```erlang
-module(myapp_auth_handler).

%% This simple handler module allows `user` with password `pass` 
%% be authorized for socks4 and `root` with any password for both protocols.
%% It rejects any other users.

-export([auth/4]).

auth(socks4, <<"user">>, <<"pass">>, _Options) -> ok.

auth(_Proto, <<"root">>, _Password, _Options) -> ok.

auth(_Proto, _User, _Password, _Options) -> false.
```

### Testing

Build:

    $ git clone https://github.com/surik/tunnerl.git
    $ cd tunnerl
    $ rebar3 compile

Run tunnerl with simple predefined configuration with socks4/socks5 and no authorization:

    $ rebar3 shell --name socks@127.0.0.1 --config tunnerl.config --apps tunnerl

There is a bunch of common tests which can be running:

    $ rebar3 ct

**Note** that IPv6 tests might not been working on your local machine.
