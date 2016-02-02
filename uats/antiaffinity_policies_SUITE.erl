-module(antiaffinity_policies_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_antiaffinity_returns_expected_policies/1,
         clc_v2_antiaffinity_returns_a_single_policy/1,
         clc_v2_antiaffinity_creates_expected_policy/1,
         clc_v2_antiaffinity_updates_expected_policy/1,
         clc_v2_antiaffinity_deletes_expected_policy/1
        ]).

all() -> [clc_v2_antiaffinity_returns_expected_policies,
          clc_v2_antiaffinity_returns_a_single_policy,
          clc_v2_antiaffinity_creates_expected_policy,
          clc_v2_antiaffinity_updates_expected_policy,
          clc_v2_antiaffinity_deletes_expected_policy
         ].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

clc_v2_antiaffinity_returns_expected_policies(Config) ->
  Expected = random_policies(),
  data_server:put(antiaffinity_policies, Expected),

  { ok, Actual } = clc_v2:antiaffinity_policies(?AUTH(Config)),

  assert:equal(Expected, Actual),
  ok.

clc_v2_antiaffinity_returns_a_single_policy(Config) ->
  Expected = #{<<"id">> := Id} = random_policy(),
  data_server:put(antiaffinity_policies, Id, Expected),

  { ok, Actual } = clc_v2:antiaffinity_policy(?AUTH(Config), Id),

  assert:equal(Expected, Actual),
  ok.

clc_v2_antiaffinity_creates_expected_policy(Config) ->
  Spec = #{ name => <<"p1">>, location => <<"l1">> },
  Expected = #{ <<"name">> => <<"p1">>, <<"location">> => <<"l1">> },

  { ok, Id } = clc_v2:create_antiaffinity_policy(?AUTH(Config), Spec),

  assert:equal(Expected, data_server:get(antiaffinity_policies, Id)),
  ok.

clc_v2_antiaffinity_updates_expected_policy(Config) ->
  Spec = #{name => <<"new name">> },
  Expected = #{ <<"name">> => <<"new name">> },

  Id = <<"123">>,
  ok = clc_v2:update_antiaffinity_policy(?AUTH(Config), Spec, Id),

  assert:equal(Expected, data_server:get(antiaffinity_policies, Id)),
  ok.


clc_v2_antiaffinity_deletes_expected_policy(Config) ->
  Id = <<"123">>,

  ok = clc_v2:delete_antiaffinity_policy(?AUTH(Config), Id),

  assert:equal(deleted, data_server:get(antiaffinity_policies, Id)),
  ok.

random_policies() ->
  Items = [random_policy(),
           random_policy()],

  #{ <<"items">> => Items,
     <<"links">> => #{ <<"href">> => <<"/v2/antiAffinityPolicies/Alias">>,
                       <<"rel">> => <<"self">>,
                       <<"verbs">> => [<<"GET">>,<<"POST">>] } }.

random_policy() ->
  #{ <<"id">> => ?RBIN(),
     <<"name">> => ?RBIN(),
     <<"location">> => ?RBIN(),
     <<"links">> =>
      [#{ <<"rel">> => <<"self">>,
          <<"href">> => <<"/v2/antiAffinityPolicies/Alias/Id">>,
          <<"verbs">> => [<<"GET">>,<<"PUT">>,<<"DELETE">>]
        },
       #{ <<"rel">> => <<"location">>,
          <<"href">> => <<"/v2/datacenters/Alias/Id">>,
          <<"id">> => ?RBIN(),
          <<"name">> => ?RBIN()
        }]
  }.
