-module(datacenters_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_datacenters_returns_a_list_with_ca1_in_first_element/1]).

all() -> [clc_v2_datacenters_returns_a_list_with_ca1_in_first_element].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  {ok, _Pid} = application:ensure_all_started( clc_v2 ),
  Config.

end_per_suite(_Config) ->
  application:stop( clc_v2 ).

clc_v2_datacenters_returns_a_list_with_ca1_in_first_element(_Config) ->
  AuthRef = clc_v2:login( <<"username">>, <<"password">> ),
  [ #{ <<"id">> := <<"ca1">> }  | _Tail ] = clc_v2:datacenters(AuthRef),
  ok.

