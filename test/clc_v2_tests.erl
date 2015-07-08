-module( clc_v2_tests ).
-include( "test_fixture.hrl" ).

clc_v2_datacenters_delgates_to_datacenters_get() ->
  ?meck(clc_v2_datacenters, [non_strict]),
  ?stub(clc_v2_datacenters, get, 1, datacenters1),

  ?assertEqual(datacenters1, clc_v2:datacenters(auth_ref1)),

  ?called(clc_v2_datacenters, get, [auth_ref1]).

clc_v2_datacenter_delgates_to_datacenters_get() ->
  ?meck(clc_v2_datacenters, [non_strict]),
  ?stub(clc_v2_datacenters, get, 2, datacenter1),

  ?assertEqual(datacenter1, clc_v2:datacenter(auth_ref1, datacenter_id1)),

  ?called(clc_v2_datacenters, get, [auth_ref1, datacenter_id1]).

clc_v2_alert_policies_delegates_to_alert_policies_get() ->
  ?meck(clc_v2_alert_policies, [non_strict]),
  ?stub(clc_v2_alert_policies, get, 1, alert_policies1),

  ?assertEqual(alert_policies1, clc_v2:alert_policies(auth_ref1)),

  ?called(clc_v2_alert_policies, get, [auth_ref1]).

login_creates_new_auth_worker_under_auth_supervisor() ->
  ?meck( clc_v2_auth_sup, [non_strict] ),
  ?stub( clc_v2_auth_sup, create_worker, 2, {ok, authref1}),

  ?assertEqual( authref1, clc_v2:login( username1, password1 ) ),

  ?called( clc_v2_auth_sup, create_worker, [username1, password1] ).
