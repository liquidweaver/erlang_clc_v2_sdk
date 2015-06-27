-module( clc_v2_tests ).
-include( "test_fixture.hrl" ).

clc_v2_datacenters_delgates_to_datacenters_get() ->
  ?meck(clc_v2_datacenters, [non_strict]),
  ?stub(clc_v2_datacenters, get, 0, datacenters1),

  ?assertEqual(datacenters1, clc_v2:datacenters()),

  ?called(clc_v2_datacenters, get, []).
