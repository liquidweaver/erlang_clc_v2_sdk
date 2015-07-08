-module(datacenters_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_datacenters_returns_a_list_with_ca1_in_first_element/1,
         clc_v2_datacenter_returns_a_single_datacenter/1,
         clc_v2_datacenter_capabilities_returns_capabilites_for_a_datacenter/1]).

all() -> [clc_v2_datacenters_returns_a_list_with_ca1_in_first_element,
          clc_v2_datacenter_returns_a_single_datacenter,
          clc_v2_datacenter_capabilities_returns_capabilites_for_a_datacenter].
suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?START_APP_AND_GET_AUTH_REF(Config).

end_per_suite(_Config) ->
  application:stop( clc_v2 ).

clc_v2_datacenters_returns_a_list_with_ca1_in_first_element(Config) ->
  AuthRef = proplists:get_value( auth_ref, Config ),
  Datacenters = clc_v2:datacenters(AuthRef),
  io:format( "Datacenters: ~p~n", [Datacenters] ),
  [ #{ <<"id">> := <<"ca1">> }  | _Tail ] = Datacenters,
  true = (length(Datacenters) > 1),
  ok.

clc_v2_datacenter_returns_a_single_datacenter(Config) ->
  AuthRef = proplists:get_value( auth_ref, Config ),
  Datacenter = clc_v2:datacenter(AuthRef, <<"ca1">>),
  io:format( "Datacenter: ~p~n", [Datacenter] ),
  #{ <<"id">> := <<"ca1">> } = Datacenter,
  ok.

clc_v2_datacenter_capabilities_returns_capabilites_for_a_datacenter(Config) ->
  AuthRef = proplists:get_value( auth_ref, Config ),
  Capabilities = clc_v2:datacenter_capabilities(AuthRef, <<"ca1">>),
  io:format( "Datacenter capabilities ~p~n", [Capabilities] ),
  #{ <<"supportsPremiumStorage">> := _ } = Capabilities,
  ok.
