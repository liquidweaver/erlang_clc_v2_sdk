-module(alert_policies_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_alerts_returns_expected_policies/1,
         clc_v2_alerts_returns_a_single_policy/1,
         clc_v2_alerts_creates_expected_policy/1,
         clc_v2_alerts_updates_expected_policy/1,
         clc_v2_alerts_deletes_expected_policy/1
        ]).

all() -> [clc_v2_alerts_returns_expected_policies,
          clc_v2_alerts_returns_a_single_policy,
          clc_v2_alerts_creates_expected_policy,
          clc_v2_alerts_updates_expected_policy,
          clc_v2_alerts_deletes_expected_policy
         ].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

clc_v2_alerts_returns_expected_policies(Config) ->
  Expected = random_policies(),
  data_server:put(alert_policies, Expected),

  { ok, Actual } = clc_v2:alert_policies(?AUTH(Config)),

  assert:equal(Expected, Actual),
  ok.

clc_v2_alerts_returns_a_single_policy(Config) ->
  Expected = #{<<"id">> := Id} = random_policy(),
  data_server:put(alert_policies, Id, Expected),

  { ok, Actual } = clc_v2:alert_policy(?AUTH(Config), Id),

  assert:equal(Expected, Actual),
  ok.

clc_v2_alerts_creates_expected_policy(Config) ->
  Spec = #{name => <<"p1">>,
           email_recipients => [<<"r1@host.com">>, <<"r2@host.com">>],
           triggers =>
            [ #{ metric => cpu, duration => <<"00:00:01">>, threshold => 67.8 },
              #{ metric => memory, duration => <<"01:23:45">>, threshold => 0.12 } ]
          },
  Expected = #{ <<"name">> => <<"p1">>,
                <<"actions">> =>
                  [#{ <<"action">> => <<"email">>,
                      <<"settings">> => #{ <<"recipients">> => [<<"r1@host.com">>, <<"r2@host.com">>] }
                  }],
                <<"triggers">> =>
                  [#{ <<"metric">> => <<"cpu">>, <<"duration">> => <<"00:00:01">>, <<"threshold">> => 67.8 },
                   #{ <<"metric">> => <<"memory">>, <<"duration">> => <<"01:23:45">>, <<"threshold">> => 0.12 }
                  ]
              },


  { ok, Id } = clc_v2:create_alert_policy(?AUTH(Config), Spec),

  assert:equal(Expected, data_server:get(alert_policies, Id)),
  ok.

clc_v2_alerts_updates_expected_policy(Config) ->
  Spec = #{name => <<"p1">>,
           email_recipients => [<<"r1@host.com">>],
           triggers => [ #{ metric => cpu, duration => <<"00:00:01">>, threshold => 67.8 } ]
          },

  Expected = #{ <<"name">> => <<"p1">>,
                <<"actions">> =>
                  [#{ <<"action">> => <<"email">>,
                      <<"settings">> => #{ <<"recipients">> => [<<"r1@host.com">>] }
                  }],
                <<"triggers">> =>
                  [#{ <<"metric">> => <<"cpu">>, <<"duration">> => <<"00:00:01">>, <<"threshold">> => 67.8 } ]
              },

  Id = <<"123">>,
  ok = clc_v2:update_alert_policy(?AUTH(Config), Spec, Id),

  assert:equal(Expected, data_server:get(alert_policies, Id)),
  ok.


clc_v2_alerts_deletes_expected_policy(Config) ->
  Id = <<"123">>,

  ok = clc_v2:delete_alert_policy(?AUTH(Config), Id),

  assert:equal(deleted, data_server:get(alert_policies, Id)),
  ok.

random_policies() ->
  Items = [random_policy(),
           random_policy()],

  #{ <<"items">> => Items,
     <<"links">> => #{ <<"href">> => <<"/v2/alertPolicies/Alias">>,
                       <<"rel">> => <<"self">>,
                       <<"verbs">> => ["GET","POST"] } }.

random_policy() ->
  Id = <<"id", (?RBIN())/binary>>,

  #{ <<"id">> => Id,
     <<"name">> => <<"name", (?RBIN())/binary>>,
     <<"actions">> =>
      [ #{
          <<"action">> => <<"email">>,
          <<"settings">> =>
            #{ <<"receipients">> => [<<"user", (?RBIN())/binary, "@domain.com">>,
                                     <<"user@domain", (?RBIN())/binary, ".com">>] }
        }],
     <<"triggers">> =>
      [#{
          <<"metric">> => <<"cpu">>,
          <<"duration">> => "12:34:56",
          <<"threshold">> => ?RFLOAT()
        },
       #{
          <<"metric">> => <<"disk">>,
          <<"duration">> => "01:23:45",
          <<"threshold">> => ?RFLOAT()
        }],
      <<"links">> =>
       #{
          <<"href">> => <<"/v2/alertPolicies/Alias/", Id/binary>>,
          <<"rel">> => <<"self">>,
          <<"verbs">> => ["GET","DELETE","PUT"]
        }
  }.
