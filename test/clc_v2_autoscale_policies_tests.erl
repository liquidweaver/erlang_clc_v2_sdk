-module( clc_v2_autoscale_policies_tests ).
-include( "test_fixture.hrl" ).

setup() ->
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, { ok, data1 }),
  ?stub( clc_v2_http_client, put, 3, { ok, result1 }),
  ?stub( clc_v2_http_client, delete, 2, { ok, result1 }).

get_calls_http_client_get() ->
 ?assertEqual( { ok, data1 }, clc_v2_autoscale_policies:get( auth_ref1 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["autoscalePolicies", account_alias]] ).

get_id_calls_http_client_get() ->
  ?assertEqual( { ok, data1 }, clc_v2_autoscale_policies:get( auth_ref1, <<"id1">> ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["autoscalePolicies", account_alias, "id1"]] ).

get_server_policy_calls_http_client_get() ->
  ?assertEqual( { ok, data1 }, clc_v2_autoscale_policies:get_server_policy( auth_ref1, <<"server id 1">> ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["servers", account_alias, "server id 1", "cpuAutoscalePolicy"]] ).

update_server_policy_calls_http_client_put_with_expected_route() ->
  clc_v2_autoscale_policies:update_server_policy( auth_ref1, <<"server id">>, <<>> ),

  ?called( clc_v2_http_client, put, [auth_ref1, ["servers", account_alias, "server id", "cpuAutoscalePolicy"], ?any] ).

update_server_policy_calls_http_client_put_with_expected_body() ->
  PolicyId = <<"policy id">>,
  Expected = #{ id => PolicyId },

  clc_v2_autoscale_policies:update_server_policy( auth_ref1, <<>>, PolicyId),

  ?assertEqual( Expected, ?capture(clc_v2_http_client, put, 3, 3)).

update_server_policy_returns_expected_value() ->
  ?assertEqual( ok, clc_v2_autoscale_policies:update_server_policy( auth_ref1, <<>>, <<>> ) ).

update_server_policy_returns_error_on_error() ->
  Expected = { error, <<"Reason">> },
  ?stub( clc_v2_http_client, put, 3, Expected ),

  Actual = clc_v2_autoscale_policies:update_server_policy( auth_ref1, <<>>, <<>> ),

  ?assertEqual( Expected, Actual ).

remove_server_policy_returns_expected_value() ->
  ?assertEqual( ok, clc_v2_autoscale_policies:remove_server_policy( auth_ref1, <<>> ) ).

remove_server_policy_returns_error_on_error() ->
  Expected = { error, <<"Reason">> },
  ?stub( clc_v2_http_client, delete, 2, Expected ),

  Actual = clc_v2_autoscale_policies:remove_server_policy( auth_ref1, <<>>),

  ?assertEqual( Expected, Actual ).

remove_server_policy_calls_http_client_delete_with_expected_route() ->
  clc_v2_autoscale_policies:remove_server_policy( auth_ref1, <<"server id">> ),

  ?called( clc_v2_http_client, delete, [auth_ref1, ["servers", account_alias, "server id", "cpuAutoscalePolicy"] ] ).
