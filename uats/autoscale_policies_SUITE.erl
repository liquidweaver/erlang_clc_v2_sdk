-module(autoscale_policies_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_autoscale_returns_expected_policies/1,
         clc_v2_autoscale_returns_a_single_policy/1,
         clc_v2_server_autoscale_returns_server_policy/1,
         clc_v2_server_autoscale_updates_expected_policy/1,
         clc_v2_server_autoscale_removes_expected_policy/1
        ]).

all() -> [clc_v2_autoscale_returns_expected_policies,
         clc_v2_autoscale_returns_a_single_policy,
         clc_v2_server_autoscale_returns_server_policy,
         clc_v2_server_autoscale_updates_expected_policy,
         clc_v2_server_autoscale_removes_expected_policy
        ].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

clc_v2_autoscale_returns_expected_policies(Config) ->
  Expected = random_policies(),
  data_server:put(autoscale_policies, Expected),

  { ok, Actual } = clc_v2:autoscale_policies(?AUTH(Config)),

  assert:equal(Expected, Actual),
  ok.

clc_v2_autoscale_returns_a_single_policy(Config) ->
  Expected = #{<<"id">> := Id} = random_policy(),
  data_server:put(autoscale_policies, Id, Expected),

  { ok, Actual } = clc_v2:autoscale_policy(?AUTH(Config), Id),

  assert:equal(Expected, Actual),
  ok.

clc_v2_server_autoscale_returns_server_policy(Config) ->
	ServerId = ?RBIN(),
  Expected = server_policy(),
  data_server:put(server_autoscale_policies, ServerId, Expected),

  { ok, Actual } = clc_v2:server_autoscale_policy(?AUTH(Config), ServerId),

  assert:equal(Expected, Actual),
  ok.

clc_v2_server_autoscale_updates_expected_policy(Config) ->
	ServerId = ?RBIN(),
	Expected = #{ <<"id">> := PolicyId } = #{ <<"id">> => ?RBIN() },

  ok = clc_v2:update_server_autoscale_policy(?AUTH(Config), ServerId, PolicyId),

	assert:equal(Expected, data_server:get(server_autoscale_policies, ServerId)),
  ok.

clc_v2_server_autoscale_removes_expected_policy(Config) ->
	ServerId = ?RBIN(),
	PolicyId = ?RBIN(),

  ok = clc_v2:remove_server_autoscale_policy(?AUTH(Config), ServerId),

	assert:equal(deleted, data_server:get(server_autoscale_policies, ServerId)),
  ok.

random_policies() ->
  [random_policy(), random_policy()].

random_policy() ->
	#{<<"id">> => ?RBIN(),
		<<"name">> => ?RBIN(),
		<<"resourceType">> => ?RBIN(),
    <<"thresholdPeriodMinutes">> => ?RINT(),
		<<"scaleUpIncrement">> => ?RINT(),
		<<"range">> => #{
			<<"max">> => ?RINT(),
			<<"min">> => ?RINT()
		},
		<<"scaleUpThreshold">> => ?RINT(),
		<<"scaleDownThreshold">> => ?RINT(),
		<<"scaleDownWindow">> => #{
			<<"start">> => <<"02:00">>,
			<<"end">> => <<"04:00">>
		},
		<<"links">> => [#{
				<<"rel">> => <<"self">>,
				<<"href">> => <<"/v2/autoscalePolicies/ALIAS/Id">>
			}]
	}.

server_policy() ->
	#{
		<<"id">> => ?RBIN(),
		<<"links">> => [#{
			<<"rel">> => <<"self">>,
			<<"href">> => <<"/v2/servers/Alias/ServerId/cpuAutoscalePolicy">>
		}]
	}.
