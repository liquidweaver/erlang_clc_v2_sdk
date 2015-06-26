-module( clc_v2_app_tests ).
-include( "test_fixture.hrl" ).

clc_v2_app_exists_and_starts_sup() ->
  ok = application:start(clc_v2),
  ?assertNot(undefined == whereis(clc_v2_sup)).
