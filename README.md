# Tunnerl - SOCKS Proxy Implementation in Erlang/OTP

[![Build Status](https://github.com/surik/tunnerl/actions/workflows/ci.yml/badge.svg)](https://github.com/surik/tunnerl/actions/workflows/ci.yml)

Tunnerl is a versatile SOCKS4, SOCKS4a, and SOCKS5 proxy protocol implementation in Erlang/OTP. 
It enables you to create a powerful and flexible proxy server to facilitate secure and efficient communication across various network environments.

### Features

`tunnerl`  offers a range of features for proxying TCP connections, making it a valuable tool for network communication.
These features include:

- **SOCKS Protocol Support**:
  - SOCKS4: A protocol for TCP proxy across firewalls ([socks4.protocol](https://www.openssh.com/txt/socks4.protocol)).
    - Supports the `connect` command only.
  - SOCKS 4A: A Simple Extension to SOCKS 4 Protocol ([socks4a.protocol](https://www.openssh.com/txt/socks4a.protocol)).
  - SOCKS Protocol Version 5 ([RFC1928](https://www.ietf.org/rfc/rfc1928.txt)):
    - Supports the `connect` command only.
    - Username/Password Authentication for SOCKS V5 ([RFC1929](https://tools.ietf.org/rfc/rfc1929.txt)).
    - ATYPs: IPv4, IPv6, and domain.

### Getting Started

Follow these steps to integrate Tunnerl into your Erlang or Elixir application:

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

3. Configure `tunnerl` to use custom handler::

```erlang
{tunnerl, [
    {protocols, [socks4, socks5]},
    {handler, myapp_handler},
    {acceptors, 10},
    {ip, {0, 0, 0, 0}},
    {port, 1080}
]}.
```

4. Implement your own `myapp_handler` module. 
This module defines how `tunnerl` handles authentication and connection requests based on your application's requirements.

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
auth(_) -> reject.

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

To test `tunnerl`, follow these steps:

1. Clone the `tunnerl` repository and compile the code:

```
$ git clone https://github.com/surik/tunnerl.git
$ cd tunnerl
$ rebar3 compile
```

2. Run Tunnerl with a predefined configuration that includes SOCKS4 and SOCKS5 support with no authentication:

```
$ rebar3 shell --name socks@127.0.0.1 --config tunnerl.config --apps tunnerl
```

3. You can run a series of common tests to ensure everything is working as expected:

```
$ rebar3 ct
```

**Note**: IPv6 tests may not work on all local machines.
