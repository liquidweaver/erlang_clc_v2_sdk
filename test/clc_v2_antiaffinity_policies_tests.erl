-module( clc_v2_antiaffinity_policies_tests ).
-include( "test_fixture.hrl" ).
-define(EMPTY_POLICY(), #{ name => <<>>, location => <<>> } ).

setup() ->
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, { ok, data1 }),
  ?stub( clc_v2_http_client, post, 3, { ok, #{ <<"id">> => <<"id1">>, <<"other">> => <<"other">> } }),
  ?stub( clc_v2_http_client, put, 3, { ok, result1 }),
  ?stub( clc_v2_http_client, delete, 2, { ok, result1 }).

get_calls_http_client_get() ->
  ?assertEqual( { ok, data1 }, clc_v2_antiaffinity_policies:get( auth_ref1 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["antiAffinityPolicies", account_alias]] ).

get_id_calls_http_client_get() ->
  ?assertEqual( { ok, data1 }, clc_v2_antiaffinity_policies:get( auth_ref1, <<"id1">> ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["antiAffinityPolicies", account_alias, "id1"]] ).

create_calls_http_client_post_with_expected_route() ->
  clc_v2_antiaffinity_policies:create( auth_ref1, ?EMPTY_POLICY() ),

  ?called( clc_v2_http_client, post, [auth_ref1, ["antiAffinityPolicies", account_alias], ?any] ).

create_calls_http_client_post_with_expected_body() ->
  Spec = #{ name => <<"p1">>, location => <<"l1">> },

  clc_v2_antiaffinity_policies:create( auth_ref1, Spec ),

  ?assertEqual( Spec, ?capture(clc_v2_http_client, post, 3, 3) ).

create_returns_expected_value() ->
  ?assertEqual({ok, <<"id1">>}, clc_v2_antiaffinity_policies:create( auth_ref1, ?EMPTY_POLICY()) ).

create_returns_error_on_error() ->
  Error = { error, <<"Reason">> },
  ?stub( clc_v2_http_client, post, 3, Error),

  Actual = clc_v2_antiaffinity_policies:create( auth_ref1, ?EMPTY_POLICY() ),

  ?assertEqual(Error, Actual).

update_calls_http_client_put_with_expected_route() ->
  clc_v2_antiaffinity_policies:update( auth_ref1, ?EMPTY_POLICY(), <<"id1">> ),

  ?called( clc_v2_http_client, put, [auth_ref1, ["antiAffinityPolicies", account_alias, "id1"], ?any] ).

update_calls_http_client_post_with_expected_body() ->
  Spec = #{ name => <<"new name">> },

  clc_v2_antiaffinity_policies:update( auth_ref1, Spec, <<>>),

  ?assertEqual( Spec, ?capture(clc_v2_http_client, put, 3, 3)).

update_returns_expected_value() ->
  ?assertEqual( ok, clc_v2_antiaffinity_policies:update( auth_ref1, ?EMPTY_POLICY(), <<>> ) ).

update_returns_error_on_error() ->
  Expected = { error, <<"Reason">> },
  ?stub( clc_v2_http_client, put, 3, Expected ),

  Actual = clc_v2_antiaffinity_policies:update( auth_ref1, ?EMPTY_POLICY(), <<>> ),

  ?assertEqual( Expected, Actual ).

delete_returns_expected_value() ->
  ?assertEqual( ok, clc_v2_antiaffinity_policies:delete( auth_ref1, <<"id1">> ) ).

delete_returns_error_on_error() ->
  Expected = { error, <<"Reason">> },
  ?stub( clc_v2_http_client, delete, 2, Expected ),

  Actual = clc_v2_antiaffinity_policies:delete( auth_ref1, <<"id1">>),

  ?assertEqual( Expected, Actual ).

delete_calls_http_client_delete_with_expected_route() ->
  clc_v2_antiaffinity_policies:delete( auth_ref1, <<"id1">> ),

  ?called( clc_v2_http_client, delete, [auth_ref1, ["antiAffinityPolicies", account_alias, "id1"] ] ).
