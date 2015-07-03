-module(datacenters_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([clc_v2_datacenters_returns_a_list_with_ca1_in_first_element/1]).

all() -> [clc_v2_datacenters_returns_a_list_with_ca1_in_first_element].

clc_v2_datacenters_returns_a_list_with_ca1_in_first_element(_Config) ->
  {ok, _Pid} = application:ensure_all_started( clc_v2 ),
  AuthRef = clc_v2:login( <<"username">>, <<"password">> ),
  [ #{ <<"id">> := <<"ca1">> }  | _Tail ] = clc_v2:datacenters(AuthRef),
  application:stop( clc_v2 ).

