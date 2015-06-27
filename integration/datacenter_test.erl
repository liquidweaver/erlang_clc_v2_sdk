-module( integration_test ).
-include( "test_fixture.hrl" ).

setup() ->
  application:start( clc_v2 ).

clc_v2_datacenters_returns_a_list_with_ca1_in_first_element() ->
  {ok, AuthRef } = clc_v2:login( <<"username">>, <<"password">> ),
  ?assertMatch( [ #{ <<"id">> := <<"ca1">> }  | _Ignore ], clc_v2:datacenters(AuthRef) ).
