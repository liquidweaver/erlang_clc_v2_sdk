-module(datacenters_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, end_per_testcase/1, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_datacenters_returns_expected_datacenters/1,
         clc_v2_datacenter_returns_a_single_datacenter/1,
         clc_v2_datacenter_deployment_capabilities_returns_capabilites_for_a_datacenter/1,
         clc_v2_datacenter_baremetal_capabilities_returns_capabilites_for_a_datacenter/1]).

all() -> [clc_v2_datacenters_returns_expected_datacenters,
          clc_v2_datacenter_returns_a_single_datacenter,
          clc_v2_datacenter_deployment_capabilities_returns_capabilites_for_a_datacenter,
          clc_v2_datacenter_baremetal_capabilities_returns_capabilites_for_a_datacenter].
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
  Expected = deployment_capabilities(),
  data_server:put(datacenter_deployment_capabilities, Datacenter, Expected),

  AuthRef = proplists:get_value( auth_ref, Config ),
  { ok, Actual } = clc_v2:datacenter_deployment_capabilities(AuthRef, Datacenter),

  assert:equal(Expected, Actual),
  ok.

clc_v2_datacenter_baremetal_capabilities_returns_capabilites_for_a_datacenter(Config) ->
  Datacenter = <<"dc1">>,
  Expected = baremetal_capabilities(),
  data_server:put(datacenter_baremetal_capabilities, Datacenter, Expected),

  AuthRef = proplists:get_value( auth_ref, Config ),
  { ok, Actual } = clc_v2:datacenter_baremetal_capabilities(AuthRef, Datacenter),

  assert:equal(Expected, Actual),
  ok.

random_datacenter() ->
  Id = <<"id", (?RBIN())/binary>>,
  LinkSuffix = <<"Alias/", Id/binary>>,
  Link = <<"/v2/datacenters/", LinkSuffix/binary>>,

  #{ <<"id">> => Id,
     <<"name">> => <<"name", (?RBIN())/binary>>,
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

deployment_capabilities() ->
  #{ <<"dataCenterEnabled">> => true,
     <<"importVMEnabled">> => true,
     <<"supportsPremiumStorage">> => false,
     <<"supportsSharedLoadBalancer">> => true,
     <<"supportsBareMetalServers">> => true,
     <<"deployableNetworks">> => [],
     <<"importableOSTypes">> =>
      [#{ <<"id">> => ?RINT(),
         <<"description">> => <<"123">>,
         <<"labProductCode">> => <<"LPC123">>,
         <<"premiumProductCode">> => <<"PPC123">>,
         <<"type">> => <<"OSA_BBit">> },
       #{ <<"id">> => ?RINT(),
         <<"description">> => <<"456">>,
         <<"labProductCode">> => <<"LPC456">>,
         <<"premiumProductCode">> => <<"PPC456">>,
         <<"type">> => <<"OSC_DBit">> }],
    <<"templates">> =>
      [#{ <<"name">> => ?RBIN(),
          <<"osType">> => <<"OSA_BBit">>,
          <<"description">> => <<"Template1 Description">>,
          <<"storageSizeGB">> => ?RINT(),
          <<"capabilities">> => [<<"cpuAutoscale">>],
          <<"reservedDrivePaths">> => ["bin", "boot", "dev"] },
      #{ <<"name">> => ?RBIN(),
          <<"osType">> => <<"OSC_DBit">>,
          <<"description">> => <<"Template2 Description">>,
          <<"storageSizeGB">> => ?RINT(),
          <<"capabilities">> => [<<"cpuAutoscale">>],
          <<"reservedDrivePaths">> => ["bin", "boot", "dev"] }
      ]
   }.

baremetal_capabilities() ->
	#{<<"skus">> =>
		[	#{<<"id">> => <<"529e2592a3e640a7c2617b5e8bc8feaed95eab22">>,
				<<"hourlyRate">> => 0.56,
				<<"availability">> => <<"high">>,
				<<"memory">> => [#{ <<"capacityGB">> => 16 }],
				<<"processor">> =>
					#{	<<"coresPerSocket">> => 4,
							<<"description">> => <<"Intel(R) Xeon(R) CPU E3-1271 v3 @ 3.60GHz">>,
							<<"sockets">> => 1 },
				<<"storage">> =>
					[	#{<<"capacityGB">> => 1000,
							<<"speedRpm">> => 7200,
							<<"type">> => <<"Hdd">> } ]
			},
			#{<<"id">> => <<"f24b18ba2ce23657657444601649c7b8b7f9b60c">>,
				<<"hourlyRate">> => 1.65,
				<<"availability">> => <<"none">>,
				<<"memory">> => [#{ <<"capacityGB">> =>  64 }],
				<<"processor">> =>
					#{	<<"coresPerSocket">> => 6,
							<<"description">> => <<"Intel(R) Xeon(R) CPU E5-2620 v3 @ 2.40GHz">>,
							<<"sockets">> => 2 },
				<<"storage">> =>
					[	#{<<"capacityGB">> => 2000,
							<<"speedRpm">> => 7200,
							<<"type">> => <<"Hdd">>	},
						#{<<"capacityGB">> => 2000,
							<<"speedRpm">> => 7200,
							<<"type">> => <<"Hdd">>	}	]
			}
		],
	<<"operatingSystems">> =>
		[	#{<<"type">> => <<"centOS6_64Bit">>,
				<<"description">> => <<"CentOS 6 64-bit">>,
				<<"hourlyRatePerSocket">> => 0	},
			#{<<"type">> => <<"redHat6_64Bit">>,
				<<"description">> => <<"RedHat Enterprise Linux 6 64-bit">>,
				<<"hourlyRatePerSocket">> => 0.075	}
		]
	}.
