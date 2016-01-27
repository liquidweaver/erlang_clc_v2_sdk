-module( clc_v2_tests ).
-include( "test_fixture.hrl" ).

clc_v2_datacenter_deployment_capabilites_delegates_to_datacenters_deployment_capabilities() ->
  ?meck(clc_v2_datacenters, [non_strict]),
  ?stub(clc_v2_datacenters, deployment_capabilities, 2, datacenter1),

  ?assertEqual(datacenter1, clc_v2:datacenter_deployment_capabilities(auth_ref1, datacenter_id1)),

  ?called(clc_v2_datacenters, deployment_capabilities, [auth_ref1, datacenter_id1]).

clc_v2_datacenter_baremetal_capabilites_delegates_to_datacenters_baremetal_capabilities() ->
  ?meck(clc_v2_datacenters, [non_strict]),
  ?stub(clc_v2_datacenters, baremetal_capabilities, 2, datacenter1),

  ?assertEqual(datacenter1, clc_v2:datacenter_baremetal_capabilities(auth_ref1, datacenter_id1)),

  ?called(clc_v2_datacenters, baremetal_capabilities, [auth_ref1, datacenter_id1]).

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

clc_v2_alert_policy_delegates_to_alert_policies_get() ->
  ?meck(clc_v2_alert_policies, [non_strict]),
  ?stub(clc_v2_alert_policies, get, 2, alert_policy1),

  ?assertEqual(alert_policy1, clc_v2:alert_policy(auth_ref1, id1)),

  ?called(clc_v2_alert_policies, get, [auth_ref1, id1]).

clc_v2_create_alert_policy_delegates_to_alert_policies_create() ->
  ?meck(clc_v2_alert_policies, [non_strict]),
  ?stub(clc_v2_alert_policies, create, 2, id1),

  ?assertEqual(id1, clc_v2:create_alert_policy(auth_ref1, spec1)),

  ?called(clc_v2_alert_policies, create, [auth_ref1, spec1]).

clc_v2_update_alert_policy_delegates_to_alert_policies_update() ->
  ?meck(clc_v2_alert_policies, [non_strict]),
  ?stub(clc_v2_alert_policies, update, 3, result1),

  ?assertEqual(result1, clc_v2:update_alert_policy(auth_ref1, spec1, id1)),

  ?called(clc_v2_alert_policies, update, [auth_ref1, spec1, id1]).

clc_v2_delete_alert_policy_delegates_to_alert_policies_delete() ->
  ?meck(clc_v2_alert_policies, [non_strict]),
  ?stub(clc_v2_alert_policies, delete, 2, result1),

  ?assertEqual(result1, clc_v2:delete_alert_policy(auth_ref1, id1)),

  ?called(clc_v2_alert_policies, delete, [auth_ref1, id1]).

clc_v2_antiaffinity_policies_delegates_to_antiaffinity_policies_get() ->
  ?meck(clc_v2_antiaffinity_policies, [non_strict]),
  ?stub(clc_v2_antiaffinity_policies, get, 1, antiaffinity_policies1),

  ?assertEqual(antiaffinity_policies1, clc_v2:antiaffinity_policies(auth_ref1)),

  ?called(clc_v2_antiaffinity_policies, get, [auth_ref1]).

clc_v2_antiaffinity_policy_delegates_to_antiaffinity_policies_get() ->
  ?meck(clc_v2_antiaffinity_policies, [non_strict]),
  ?stub(clc_v2_antiaffinity_policies, get, 2, antiaffinity_policy1),

  ?assertEqual(antiaffinity_policy1, clc_v2:antiaffinity_policy(auth_ref1, id1)),

  ?called(clc_v2_antiaffinity_policies, get, [auth_ref1, id1]).

clc_v2_create_antiaffinity_policy_delegates_to_antiaffinity_policies_create() ->
  ?meck(clc_v2_antiaffinity_policies, [non_strict]),
  ?stub(clc_v2_antiaffinity_policies, create, 2, id1),

  ?assertEqual(id1, clc_v2:create_antiaffinity_policy(auth_ref1, spec1)),

  ?called(clc_v2_antiaffinity_policies, create, [auth_ref1, spec1]).

clc_v2_update_antiaffinity_policy_delegates_to_antiaffinity_policies_update() ->
  ?meck(clc_v2_antiaffinity_policies, [non_strict]),
  ?stub(clc_v2_antiaffinity_policies, update, 3, result1),

  ?assertEqual(result1, clc_v2:update_antiaffinity_policy(auth_ref1, spec1, id1)),

  ?called(clc_v2_antiaffinity_policies, update, [auth_ref1, spec1, id1]).

clc_v2_delete_antiaffinity_policy_delegates_to_antiaffinity_policies_delete() ->
  ?meck(clc_v2_antiaffinity_policies, [non_strict]),
  ?stub(clc_v2_antiaffinity_policies, delete, 2, result1),

  ?assertEqual(result1, clc_v2:delete_antiaffinity_policy(auth_ref1, id1)),

  ?called(clc_v2_antiaffinity_policies, delete, [auth_ref1, id1]).

login_creates_new_auth_worker_under_auth_supervisor() ->
  Expected = { ok, authref1 },
  ?meck( clc_v2_auth_sup, [non_strict] ),
  ?stub( clc_v2_auth_sup, create_worker, 2, Expected),

  ?assertEqual( Expected, clc_v2:login( username1, password1 ) ),

  ?called( clc_v2_auth_sup, create_worker, [username1, password1] ).
