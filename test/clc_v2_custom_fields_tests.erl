-module( clc_v2_custom_fields_tests ).
-include( "test_fixture.hrl" ).

get_calls_http_client_get() ->
  Expected = { ok, fields1 },
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, Expected ),

  ?assertEqual( Expected, clc_v2_custom_fields:get( auth_ref1 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["accounts", account_alias, "customFields"]] ).
