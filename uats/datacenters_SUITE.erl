-module(datacenters_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, end_per_testcase/1, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_datacenters_returns_expected_datacenters/1,
         clc_v2_datacenter_returns_a_single_datacenter/1,
         clc_v2_datacenter_deployment_capabilities_returns_capabilites_for_a_datacenter/1]).

all() -> [clc_v2_datacenters_returns_expected_datacenters,
          clc_v2_datacenter_returns_a_single_datacenter,
          clc_v2_datacenter_deployment_capabilities_returns_capabilites_for_a_datacenter].
suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

end_per_testcase(Config) ->
  ?TEST_TEARDOWN().

clc_v2_datacenters_returns_expected_datacenters(Config) ->
  Expected = [random_datacenter(), random_datacenter()],
  data_server:put(datacenters, Expected),

  AuthRef = proplists:get_value( auth_ref, Config ),
  { ok, Actual } = clc_v2:datacenters(AuthRef),

  assert:equal(Expected, Actual),
  ok.

clc_v2_datacenter_returns_a_single_datacenter(Config) ->
  Expected = #{<<"id">> := Id} = random_datacenter(),
  data_server:put(datacenters, Id, Expected),

  AuthRef = proplists:get_value( auth_ref, Config ),
  { ok, Actual } = clc_v2:datacenter(AuthRef, Id),

  assert:equal(Expected, Actual),
  ok.

clc_v2_datacenter_deployment_capabilities_returns_capabilites_for_a_datacenter(Config) ->
  Datacenter = <<"dc1">>,
  Expected = random_deployment_capabilities(),
  data_server:put(datacenter_deployment_capabilities, Datacenter, Expected),

  AuthRef = proplists:get_value( auth_ref, Config ),
  { ok, Actual } = clc_v2:datacenter_deployment_capabilities(AuthRef, Datacenter),

  assert:equal(Expected, Actual),
  ok.

random_datacenter() ->
  {Float, Bin} = ?RANDOMS(),
  Id = <<"id", Bin/binary>>,
  LinkSuffix = <<"Alias/", Id/binary>>,
  Link = <<"/v2/datacenters/", LinkSuffix/binary>>,

  #{ <<"id">> => Id,
     <<"name">> => <<"name", Bin/binary>>,
     <<"links">> =>
      [#{ <<"href">> => Link,
          <<"ref">> => <<"self">> },
       #{ <<"href">> => <<Link/binary, "/deploymentCapabilities">>,
          <<"ref">> => <<"deploymentCapabilities">> },
       #{ <<"href">> => <<"/v2/sharedLoadBalancers/", LinkSuffix/binary>>,
          <<"ref">> => <<"loadBalancers">>,
          <<"verbs">> => ["GET","POST"] },
       #{ <<"href">> => <<Link/binary, "/computeLimits">>,
          <<"ref">> => <<"computeLimits">>,
          <<"verbs">> => ["GET","POST"] },
       #{ <<"href">> => <<Link/binary, "/networkLimits">>,
          <<"ref">> => <<"networkLimits">>,
          <<"verbs">> => ["GET"] },
       #{ <<"href">> => <<"/v2/vmImport/", LinkSuffix/binary, "/available">>,
          <<"ref">> => <<"availableOvfs">>,
          <<"verbs">> => ["GET"] }]
   }.

random_deployment_capabilities() ->
  #{ <<"dataCenterEnabled">> => true,
     <<"importVMEnabled">> => true,
     <<"supportsPremiumStorage">> => true,
     <<"supportsSharedLoadBalancer">> => true,
     <<"supportsBareMetalServers">> => true,
     <<"deployableNetworks">> => [],
     <<"importableOSTypes">> =>
      [#{ <<"id">> => 123,
         <<"description">> => <<"123">>,
         <<"labProductCode">> => <<"LPC123">>,
         <<"premiumProductCode">> => <<"PPC123">>,
         <<"type">> => <<"OSA_BBit">> },
       #{ <<"id">> => 456,
         <<"description">> => <<"456">>,
         <<"labProductCode">> => <<"LPC456">>,
         <<"premiumProductCode">> => <<"PPC456">>,
         <<"type">> => <<"OSC_DBit">> }],
    <<"templates">> =>
      [#{ <<"name">> => <<"Template1">>,
          <<"osType">> => <<"OSA_BBit">>,
          <<"description">> => <<"Template1 Description">>,
          <<"storageSizeGB">> => 0,
          <<"capabilities">> => [<<"cpuAutoscale">>],
          <<"reservedDrivePaths">> => ["bin", "boot", "dev"] },
      #{ <<"name">> => <<"Template2">>,
          <<"osType">> => <<"OSC_DBit">>,
          <<"description">> => <<"Template2 Description">>,
          <<"storageSizeGB">> => 0,
          <<"capabilities">> => [<<"cpuAutoscale">>],
          <<"reservedDrivePaths">> => ["bin", "boot", "dev"] }
      ]
   }.
