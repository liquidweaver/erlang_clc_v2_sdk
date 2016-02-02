-module(assert).
-export([equal/2]).

equal(Expected, Expected) ->
  ok;
equal(Expected, Actual) ->
  ct:pal("  expected: ~p~n    actual: ~p~n", [Expected, Actual]),
  Expected = Actual.


