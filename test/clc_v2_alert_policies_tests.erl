-module( clc_v2_alert_policies_tests ).
-include( "test_fixture.hrl" ).

%https://www.centurylinkcloud.com/api-docs/v2/#alert-policies-get-alert-policies
get_calls_http_client_get() ->
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, { ok, data1 }),

  ?assertEqual( data1, clc_v2_alert_policies:get( auth_ref1 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["alertPolicies", account_alias]] ).
