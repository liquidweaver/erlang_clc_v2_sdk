-module( clc_v2_authentication_tests ).
-include( "test_fixture.hrl" ).

setup() ->
  ?meck( clc_v2_http_client, [non_strict] ),
  ?stub( clc_v2_http_client, post, 3, user_info1 ).

login_calls_http_client_post_with_authentication_route() ->
  ?assertEqual( user_info1, clc_v2_authentication:login( username1, password1 )),

  ?called( clc_v2_http_client, post, [undefined, ["authentication","login"], ?any]).

login_wraps_username_and_password_in_json_body() ->
  clc_v2_authentication:login( username1, password1 ),

  ?called( clc_v2_http_client, post, [?any, ?any, #{username => username1, password => password1}]).

login_returns_result_of_clc_v2_authentication_login() ->
  ?assertEqual( user_info1, clc_v2_authentication:login( username1, password1 )).

account_alias_acts_as_a_lens_into_user_info() ->
  UserInfoMap = #{<<"accountAlias">> => <<"ALIAS">>,
                  <<"bearerToken">> => <<"[LONG TOKEN VALUE]">>,
                  <<"locationAlias">> => <<"DC1">>,
                  <<"roles">> => [<<"AccountAdmin">>,<<"ServerAdmin">>],
                  <<"userName">> => <<"user@email.com">>},

  ?assertEqual( "ALIAS", clc_v2_authentication:account_alias(UserInfoMap) ).

bearer_token_acts_as_a_lens_into_user_info() ->
  UserInfoMap = #{<<"accountAlias">> => <<"ALIAS">>,
                  <<"bearerToken">> => <<"[LONG TOKEN VALUE]">>,
                  <<"locationAlias">> => <<"DC1">>,
                  <<"roles">> => [<<"AccountAdmin">>,<<"ServerAdmin">>],
                  <<"userName">> => <<"user@email.com">>},

  ?assertEqual( "[LONG TOKEN VALUE]", clc_v2_authentication:bearer_token(UserInfoMap) ).
