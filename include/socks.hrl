-record(state, {
    incoming_socket :: gen_tcp:socket(),
    outgoing_socket :: gen_tcp:socket(),
    client_ip :: inet:ip_address(),
    client_port :: inet:port_number(),
    auth_methods :: list(),
    socks4 :: boolean(),
    socks5 :: boolean(),
    transport :: module()
}).

-define(TIMEOUT, timer:seconds(5)).

-define(VERSION4, 16#04).
-define(VERSION5, 16#05).
