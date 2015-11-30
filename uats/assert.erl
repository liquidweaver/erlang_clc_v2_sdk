-module(assert).
-export([equal/2]).

equal(Expected, Actual) when is_map(Expected) ->
  Keys = maps:keys(Expected),
  ValuesEqual = fun(Key) ->
                    Expected1 = maps:get(Key,Expected),
                    Actual1 = maps:get(Key,Actual),
                    equal(Expected1,Actual1)
                end,
  lists:all(ValuesEqual, Keys);
equal([Expected | ExpectedTail], [Actual | ActualTail])
                                  when is_integer(Expected) =:= false ->
  equal(Expected, Actual),
  equal(ExpectedTail, ActualTail);
equal([],[]) ->
  true;
equal(Expected, Actual) ->
  ct:pal("  expected: ~p~n    actual: ~p~n", [Expected,Actual]),
  Expected =:= Actual.


