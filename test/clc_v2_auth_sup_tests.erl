-module( clc_v2_auth_sup_tests ).
-include( "test_fixture.hrl" ).


init_returns_correct_child_strategy() ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 5,
               period => 10},
  ChildSpecs = [#{id => clc_v2_auth,
                  start => {clc_v2_auth, start_link, []}
                 }],

  ?assertEqual( {ok, {SupFlags, ChildSpecs}}, clc_v2_auth_sup:init([]) ).

create_worker_spawns_a_clc_v2_auth_worker() ->
  ?meck(supervisor, [unstick]),
  ?stub(supervisor, start_child, 2, child1),

  ?assertEqual( child1, clc_v2_auth_sup:create_worker(username1, password1) ),

  ?called( supervisor, start_child, [clc_v2_auth_sup, [username1, password1]] ).
