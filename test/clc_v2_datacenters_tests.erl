-module( clc_v2_datacenters_tests ).
-include( "test_fixture.hrl" ).

% https://www.centurylinkcloud.com/api-docs/v2/#data-centers
get_calls_http_client_get() ->
  Expected = { ok, data1 },
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, Expected ),

  ?assertEqual( Expected, clc_v2_datacenters:get( auth_ref1 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["datacenters", account_alias]] ).

get_with_datacenter_id_converts_datacenter_id_to_a_list_and_calls_http_client_get() ->
  Expected = { ok, data1 },
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, Expected ),

  ?assertEqual( Expected, clc_v2_datacenters:get( auth_ref1, <<"dc1">> ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["datacenters", account_alias, "dc1"]] ).

deployment_capabilities_with_datacenter_id_converts_datacenter_id_to_a_list_and_calls_http_client_get() ->
  Expected = { ok, data1 },
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, Expected ),

  ?assertEqual( Expected, clc_v2_datacenters:deployment_capabilities( auth_ref1, <<"dc1">> ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["datacenters", account_alias, "dc1","deploymentCapabilities"]] ).

baremetal_capabilities_with_datacenter_id_converts_datacenter_id_to_a_list_and_calls_http_client_get() ->
  Expected = { ok, data1 },
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, Expected ),

  ?assertEqual( Expected, clc_v2_datacenters:baremetal_capabilities( auth_ref1, <<"dc1">> ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["datacenters", account_alias, "dc1","bareMetalCapabilities"]] ).
