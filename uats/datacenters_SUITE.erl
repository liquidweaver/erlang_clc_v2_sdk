-module(datacenters_SUITE).
-include("uat_helper.hrl").
-include("mock_clc/include/data.hrl").
-export([all/0, suite/0, end_per_testcase/1, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_datacenters_returns_a_list_with_ca1_in_first_element/1,
         clc_v2_datacenter_returns_a_single_datacenter/1,
         clc_v2_datacenter_capabilities_returns_capabilites_for_a_datacenter/1]).

all() -> [clc_v2_datacenters_returns_a_list_with_ca1_in_first_element,
          clc_v2_datacenter_returns_a_single_datacenter,
          clc_v2_datacenter_capabilities_returns_capabilites_for_a_datacenter].
suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

end_per_testcase(Config) ->
  ?TEST_TEARDOWN().

clc_v2_datacenters_returns_a_list_with_ca1_in_first_element(Config) ->
  Expected = [random_datacenter(), random_datacenter()],
  data_server:put(datacenters, Expected),
  AuthRef = proplists:get_value( auth_ref, Config ),

  Actual = clc_v2:datacenters(AuthRef),

  assert:equal(Expected, Actual),
  ok.

clc_v2_datacenter_returns_a_single_datacenter(Config) ->
  AuthRef = proplists:get_value( auth_ref, Config ),
  Datacenter = clc_v2:datacenter(AuthRef, <<"ca1">>),
  io:format( "Datacenter: ~p~n", [Datacenter] ),
  #{ <<"id">> := <<"ca1">> } = Datacenter,
  ok.

clc_v2_datacenter_capabilities_returns_capabilites_for_a_datacenter(Config) ->
  AuthRef = proplists:get_value( auth_ref, Config ),
  Capabilities = clc_v2:datacenter_capabilities(AuthRef, <<"ca1">>),
  io:format( "Datacenter capabilities ~p~n", [Capabilities] ),
  #{ <<"supportsPremiumStorage">> := _ } = Capabilities,
  ok.

random_datacenter() ->
  {Float, Bin} = ?RANDOMS(),
  Id = <<"id", Bin/binary>>,
  LinkSuffix = <<?ALIAS/binary, "/", Id/binary>>,
  Link = <<"/v2/datacenters/", LinkSuffix/binary>>,

  #{ <<"id">> => <<"id", Id/binary>>,
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
       #{ <<"href">> => <<Link/binary, "/networLimits">>,
          <<"ref">> => <<"networkLimits">>,
          <<"verbs">> => ["GET"] },
       #{ <<"href">> => <<"/v2/vmImport/", LinkSuffix/binary, "/available">>,
          <<"ref">> => <<"availableOvfs">>,
          <<"verbs">> => ["GET"] }]
   }.
