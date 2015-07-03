-module(datacenters_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_datacenters_returns_a_list_with_ca1_in_first_element/1,
         clc_v2_datacenter_returns_a_single_datacenter/1]).

all() -> [clc_v2_datacenters_returns_a_list_with_ca1_in_first_element,
          clc_v2_datacenter_returns_a_single_datacenter].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  {ok, _Pid} = application:ensure_all_started( clc_v2 ),
  Config.

end_per_suite(_Config) ->
  application:stop( clc_v2 ).

clc_v2_datacenters_returns_a_list_with_ca1_in_first_element(_Config) ->
  AuthRef = clc_v2:login( <<"username">>, <<"password">> ),
  Datacenters = clc_v2:datacenters(AuthRef),
  io:format( "Datacenters: ~p~n", [Datacenters] ),
  [ #{ <<"id">> := <<"ca1">> }  | _Tail ] = Datacenters,
  true = (length(Datacenters) > 1),
  ok.

clc_v2_datacenter_returns_a_single_datacenter(_Config) ->
  AuthRef = clc_v2:login( <<"username">>, <<"password">> ),
  Datacenter = clc_v2:datacenter(AuthRef, <<"ca1">>),
  io:format( "Datacenter: ~p~n", [Datacenter] ),
  #{ <<"id">> := <<"ca1">> } = Datacenter,
  ok.
