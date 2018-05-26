-module(tunnerl_gen_handler).

% #{
%  protocol => socks4 | socks5,
%
%  command => auth | connect | bind | udp addociate,
%
%  method => tunnerl:method()
%
%  username => binary(),
%  password => binary(),
%
%  source_ip   => inet:ip_address(),
%  source_port => inet:port_number(),
%
%  destination_address => inet:hostname() | inet:ip_address(),
%  destination_port    => inet:port_number(),
% }.
-type request() :: map().

-type reply() :: accept 
               | reject.

-type method() :: noauth 
                | username
                | gssapi
                | undefined
                | 3..254.

-type methods() :: list(method()).

% socks5 only
% list of authentication methods are used  subnegotiation 
-callback auth_methods() -> 
    methods().

% socks5 only
% accept or reject an attempt of authentication
-callback auth(Request :: request()) ->
    reply().

% socks4 and socks5
% accept or reject and attempt to evaluate command
-callback handle_command(Request :: request()) ->
    reply().
