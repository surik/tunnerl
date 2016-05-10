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
         unsuccesful_request_with_not_available_user/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, socks4_auth}, 
     {group, socks5_auth}, 
     {group, socks4_no_auth}, 
     {group, socks5_no_auth}].

groups() -> 
    [{socks4_auth, [sequence], auth_cases()},
     {socks5_auth, [sequence], auth_cases()},
     {socks4_no_auth, [sequence], noauth_cases()},
     {socks5_no_auth, [sequence], noauth_cases()}].

auth_cases() ->
    [unsuccesful_request_without_user, 
     succesful_request_with_available_user, 
     unsuccesful_request_with_not_available_user].

noauth_cases() ->
    [succesful_request_without_user, 
     succesful_request_with_available_user, 
     succesful_request_with_not_available_user].

init_per_suite(Config) ->
    application:set_env(tunnerl, protocols, [socks4, socks5]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Group, Config) when Group == socks4_auth orelse Group == socks5_auth ->
    application:set_env(tunnerl, auth_module, auth_mod),
    application:set_env(tunnerl, auth, [16#02]), % username auth for socks5
    {ok, _} = application:ensure_all_started(tunnerl),
    set_type(Group, Config);
init_per_group(Group, Config) ->
    application:set_env(tunnerl, auth_module, tunnerl_auth_dummy),
    application:set_env(tunnerl, auth, [16#00]), % no auth for socks5
    {ok, _} = application:ensure_all_started(tunnerl),
    set_type(Group, Config).

end_per_group(_Group, _Config) ->
    ok = application:stop(tunnerl),
    ok = application:stop(ranch),
    ok.

set_type(Group, Config) ->
    case Group of
        socks4_no_auth -> [{type, socks4} | Config];
        socks5_no_auth -> [{type, socks5} | Config];
        socks4_auth    -> [{type, socks4} | Config];
        socks5_auth    -> [{type, socks5} | Config]
    end.

get_type(Config) ->
    ?config(type, Config).

%%%===================================================================
%%% Test cases
%%%===================================================================
succesful_request_without_user(Config) ->
    true = curl:request(get_type(Config), "google.com").

succesful_request_with_available_user(Config) ->
    true = curl:request(get_type(Config), "google.com", "user", "pass").

succesful_request_with_not_available_user(Config) ->
    true = curl:request(get_type(Config), "google.com", "user1", "pass").

unsuccesful_request_without_user(Config) ->
    false = curl:request(get_type(Config), "google.com").

unsuccesful_request_with_not_available_user(Config) ->
    false = curl:request(get_type(Config), "google.com", "user1", "pass").
