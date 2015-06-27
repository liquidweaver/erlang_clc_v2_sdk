-module( clc_v2_sup_tests ).
-include( "test_fixture.hrl" ).

init_starts_with_default_supervisor_flags() ->
  {ok, {SupFlags, _}} = clc_v2_sup:init([]),
  ?assertEqual(#{}, SupFlags).

init_starts_auth_sup() ->
  AuthSpec = #{id => clc_v2_auth_sup,
                  start => {clc_v2_auth_sup, start_link, []},
                  type => supervisor
                },

  {ok, {_, ChildSpecs}} = clc_v2_sup:init([]),

  ?assert(lists:member(AuthSpec, ChildSpecs)).
