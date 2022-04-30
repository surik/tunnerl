-module(tunnerl_SUITE).

%% Common Test callbacks
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

%% Test cases
-export([succesful_request_without_user/1,
         succesful_request_with_available_user/1,
         succesful_request_with_not_available_user/1,
         unsuccesful_request_without_user/1,
         unsuccesful_request_with_not_available_user/1,
         unsuccesful_request_with_wrong_ip/1,
         unsuccesful_request_handler_crash_on_auth/1,
         unsuccesful_request_handler_crash_on_cmd/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, ipv4},
     {group, ipv6}].

groups() ->
    [{ipv4, [sequence], [
                         {socks4_auth, [sequence], auth_cases()},
                         {socks4a_auth, [sequence], auth_cases()},
                         {socks5_auth, [sequence], auth_cases()},
                         {socks4_no_auth, [sequence], noauth_cases()},
                         {socks4a_no_auth, [sequence], noauth_cases()},
                         {socks5_no_auth, [sequence], noauth_cases()}
                        ]},
     {ipv6, [sequence], [
                         {socks5_auth, [sequence], auth_cases()},
                         {socks5_no_auth, [sequence], noauth_cases()}
                        ]}
    ].

auth_cases() ->
    [unsuccesful_request_without_user,
     succesful_request_with_available_user,
     unsuccesful_request_with_not_available_user,
     unsuccesful_request_with_wrong_ip,
     unsuccesful_request_handler_crash_on_auth,
     unsuccesful_request_handler_crash_on_cmd].

noauth_cases() ->
    [succesful_request_without_user,
     succesful_request_with_available_user,
     succesful_request_with_not_available_user,
     unsuccesful_request_with_wrong_ip].

init_per_suite(Config) ->
    application:set_env(tunnerl, protocols, [socks4, socks5]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(ipv4, Config) ->
    [{family, inet} | Config];
init_per_group(ipv6, Config) ->
    case os:getenv("CI") of
        "true" ->
            {skip, "IPv6 is not available on CI"};
        _ ->
            [{family, inet6} | Config]
    end;
init_per_group(Group, Config)
  when Group == socks4_auth orelse
       Group == socks5_auth orelse
       Group == socks4a_auth ->
    application:set_env(tunnerl, handler, test_handler),
    application:set_env(tunnerl, auth, [username]),
    {ok, _} = application:ensure_all_started(tunnerl),
    set_type(Group, Config);
init_per_group(Group, Config) ->
    application:set_env(tunnerl, handler, tunnerl_handler_dummy),
    application:set_env(tunnerl, auth, [noauth]),
    {ok, _} = application:ensure_all_started(tunnerl),
    set_type(Group, Config).

end_per_group(Group, _Config) when Group == ipv4 orelse Group == ipv6 ->
    ok;
end_per_group(_Group, _Config) ->
    ok = application:stop(tunnerl),
    ok = application:stop(ranch),
    ok.

set_type(Group, Config) ->
    case Group of
        socks4_no_auth  -> [{type, socks4}  | Config];
        socks4a_no_auth -> [{type, socks4a} | Config];
        socks4_auth     -> [{type, socks4}  | Config];
        socks4a_auth    -> [{type, socks4a} | Config];
        socks5_no_auth  -> [{type, socks5}  | Config];
        socks5_auth     -> [{type, socks5}  | Config]
    end.

get_type(Config) ->
    ?config(type, Config).

get_family(Config) ->
    ?config(family, Config).

%%%===================================================================
%%% Test cases
%%%===================================================================
succesful_request_without_user(Config) ->
    true = curl:request(get_type(Config), get_family(Config), "google.com").

succesful_request_with_available_user(Config) ->
    true = curl:request(get_type(Config), get_family(Config), "google.com", "user", "pass").

succesful_request_with_not_available_user(Config) ->
    true = curl:request(get_type(Config), get_family(Config), "google.com", "user1", "pass").

unsuccesful_request_without_user(Config) ->
    false = curl:request(get_type(Config), get_family(Config), "google.com").

unsuccesful_request_with_not_available_user(Config) ->
    false = curl:request(get_type(Config), get_family(Config), "google.com", "user1", "pass").

unsuccesful_request_with_wrong_ip(Config) ->
    false = curl:request(get_type(Config), get_family(Config), "127.0.0.1", "user", "pass").

unsuccesful_request_handler_crash_on_auth(Config) ->
    false = curl:request(get_type(Config), get_family(Config), "google.com", "throw", "pass").

unsuccesful_request_handler_crash_on_cmd(Config) ->
    false = curl:request(get_type(Config), get_family(Config), "google.com", "throw2", "pass").
