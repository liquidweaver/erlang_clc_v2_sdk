-module(alert_policies_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_alerts_returns_expected_policies/1,
         clc_v2_alerts_returns_a_single_policy/1,
         clc_v2_alerts_creates_expected_policy/1,
         clc_v2_alerts_updates_expected_policy/1
        ]).

all() -> [clc_v2_alerts_returns_expected_policies,
          clc_v2_alerts_returns_a_single_policy,
          clc_v2_alerts_creates_expected_policy,
          clc_v2_alerts_updates_expected_policy
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

  Actual = clc_v2:alert_policies(proplists:get_value( auth_ref, Config )),

  assert:equal(Expected, Actual),
  ok.

clc_v2_alerts_returns_a_single_policy(Config) ->
  Expected = #{<<"id">> := Id} = random_policy(),
  data_server:put(alert_policies, Id, Expected),

  AuthRef = proplists:get_value( auth_ref, Config ),
  Actual = clc_v2:alert_policy(AuthRef, Id),

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


  AuthRef = proplists:get_value( auth_ref, Config ),
  Id = clc_v2:create_alert_policy(AuthRef, Spec),

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
  AuthRef = proplists:get_value( auth_ref, Config ),
  clc_v2:update_alert_policy(AuthRef, Spec, Id),

  assert:equal(Expected, data_server:get(alert_policies, Id)),
  ok.


random_policies() ->
  Items = [random_policy(),
           random_policy()],

  #{ <<"items">> => Items,
     <<"links">> => #{ <<"href">> => <<"/v2/alertPolicies/Alias">>,
                       <<"rel">> => <<"self">>,
                       <<"verbs">> => ["GET","POST"] } }.

random_policy() ->
  {Float, Bin} = ?RANDOMS(),
  Id = <<"id", Bin/binary>>,

  #{ <<"id">> => Id,
     <<"name">> => <<"name", Bin/binary>>,
     <<"actions">> =>
      [ #{
          <<"action">> => <<"email">>,
          <<"settings">> =>
            #{ <<"receipients">> => [<<"user", Bin/binary, "@domain.com">>,
                                     <<"user@domain", Bin/binary, ".com">>] }
        }],
     <<"triggers">> =>
      [#{
          <<"metric">> => <<"cpu">>,
          <<"duration">> => "12:34:56",
          <<"threshold">> => Float
        },
       #{
          <<"metric">> => <<"disk">>,
          <<"duration">> => "01:23:45",
          <<"threshold">> => Float
        }],
      <<"links">> =>
       #{
          <<"href">> => <<"/v2/alertPolicies/Alias/", Id/binary>>,
          <<"rel">> => <<"self">>,
          <<"verbs">> => ["GET","DELETE","PUT"]
        }
  }.
