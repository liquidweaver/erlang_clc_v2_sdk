-module(alert_policies_SUITE).
-include("uat_helper.hrl").
-include("mock_clc/include/data.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element/1]).

all() -> [clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element(Config) ->
  Expected = random_policies(),
  data_server:put(alert_policies, Expected),

  Actual = clc_v2:alert_policies(proplists:get_value( auth_ref, Config )),

  assert_equal(Expected, Actual),
  ok.

random_policies() ->
  Items = [random_policy(),
           random_policy()],

  #{ <<"items">> => Items,
     <<"links">> => #{ <<"href">> => <<"/v2/alertPolicies/", ?ALIAS/binary>>,
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
          <<"href">> => <<"/v2/alertPolicies/", ?ALIAS/binary, "/", Id/binary>>,
          <<"rel">> => <<"self">>,
          <<"verbs">> => ["GET","DELETE","PUT"]
        }
  }.

assert_equal(Expected, Actual) when is_map(Expected) ->
  Keys = maps:keys(Expected),
  ValuesEqual = fun(Key) ->
                    Expected1 = maps:get(Key,Expected),
                    Actual1 = maps:get(Key,Actual),
                    assert_equal(Expected1,Actual1)
                end,

  lists:all(ValuesEqual, Keys);
assert_equal([],[]) ->
  true;
assert_equal([Expected | ExpectedTail], [Actual | ActualTail])
                                             when is_integer(Expected) =:= false ->
  assert_equal(Expected, Actual),
  assert_equal(ExpectedTail, ActualTail);
assert_equal(Expected, Actual) ->
  Expected =:= Actual.


