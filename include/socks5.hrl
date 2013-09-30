-define(VERSION, 16#05). % only SOCKS5
-define(RSV, 16#00).
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(CMD_CONNECT, 16#01).
-define(CMD_BIND, 16#02).
-define(CMD_UDP_ASSOCIATE,  16#03).

-define(AUTH_NOAUTH, 16#00).
-define(AUTH_GSAPI, 16#01).
-define(AUTH_USERNAME, 16#02).
-define(AUTH_UNDEF, 16#FF).

-define(REP_SUCCESS, 16#00).
-define(REP_SERVER_ERROR, 16#01).
-define(REP_FIRBBIDEN, 16#02).
-define(REP_NET_NOTAVAILABLE, 16#03).
-define(REP_HOST_NOTAVAILABLE, 16#04).
-define(REP_FAILURE, 16#05).
-define(REP_TTL_EXPIRES, 16#06).
-define(REP_CMD_NOTSUPPORTED, 16#07).
-define(REP_ATYP_NOSUPPORTED, 16#08).
-define(REP_UNDEF, 16#FF).
