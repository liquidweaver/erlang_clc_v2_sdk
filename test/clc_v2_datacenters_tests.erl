-module( clc_v2_datacenters_tests ).
-include( "test_fixture.hrl" ).

% https://www.centurylinkcloud.com/api-docs/v2/#data-centers
datacenters_calls_http_client_get()->
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, { ok, data1 }),

  ?assertEqual( data1, clc_v2_datacenters:get( auth_ref1 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["datacenters", account_alias]] ).
