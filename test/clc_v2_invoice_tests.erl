-module( clc_v2_invoice_tests ).
-include( "test_fixture.hrl" ).

get_calls_http_client_get() ->
  Expected = { ok, data1 },
  ?meck( clc_v2_http_client, [non_strict]),
  ?stub( clc_v2_http_client, get, 2, Expected ),

  ?assertEqual( Expected, clc_v2_invoice:get( auth_ref1, 2016, 12 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["invoice", account_alias, "2016", "12"]] ).
