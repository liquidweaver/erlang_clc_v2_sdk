-module(alert_policies_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element/1]).

all() -> [clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?START_APP_AND_GET_AUTH_REF(Config).

end_per_suite(_Config) ->
  ?TEARDOWN().

clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element(Config) ->
  Expected = ?ITEMS([random_policy(), random_policy()]),
  data_server:put(alert_policies, Expected),

  Actual = clc_v2:alert_policies(proplists:get_value( auth_ref, Config )),

  assert_equal(Expected, Actual),
  ok.

random_policy() ->
  {Float, Bin} = ?RANDOMS(),

  #{ <<"id">> => <<"id", Bin/binary>>,
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
        }]
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


