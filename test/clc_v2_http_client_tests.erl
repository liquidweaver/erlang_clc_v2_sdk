-module( clc_v2_http_client_tests ).
-include( "test_fixture.hrl" ).

init_returns_empty_map() ->
  ?assertEqual( {ok, #{}}, clc_v2_http_client:init([]) ).

setup() ->
  ?meck( gun ),
  ?stub( gun, post, 4, ref1 ),
  ?stub( gun, await_body, 2, jiffy:encode(#{foo => bar}) ),
  ?stub( gun, open, 2, {ok, pid1} ).

handle_call_when_gun_pid_invalid_call_gun_open() ->
  DeadPid = spawn( fun() -> please_kill_me_immediately end ),
  exit( DeadPid, kill ),

  Result = clc_v2_http_client:handle_call( {get, resource1}, ignored_from, #{ gun_pid => DeadPid } ),
  ?assertMatch( { reply, _, #{ gun_pid := pid1 } }, Result ),

  ?called( gun, open, [?any, ?any] ).

handle_call_when_gun_pid_unset_call_gun_open() ->
  Result = clc_v2_http_client:handle_call( {get, resource1}, ignored_from, #{} ),
  ?assertMatch( { reply, _, #{ gun_pid := pid1 } }, Result ),

  ?called( gun, open, [?any, ?any] ).

handle_call_when_gun_pid_valid_does_not_call_gun_open() ->
  LegitPid = spawn( fun() -> receive kill_me -> ok end end ),
  Result = clc_v2_http_client:handle_call( {get, resource1}, ignored_from, #{ gun_pid => LegitPid } ),
  LegitPid ! kill_me,
  ?assertMatch( { reply, _, #{ gun_pid := LegitPid } }, Result ).

handle_call_when_auth_unset_calls_gun_post_with_auth_url_and_body() ->
  ?stub( gun, await_body, 2, <<"{\"foo\":\"bar\"}">> ),
  ExpectedJson = jiffy:encode( #{ username => <<"username">>, password => <<"password">> } ),

  clc_v2_http_client:handle_call( {get, resource1}, ignored, #{ gun_pid => self() } ),

  ?called( gun, post, [ self(), <<"v2/authentication/login">>, [], ExpectedJson ] ).

handle_call_when_auth_unset_sets_auth_to_gun_post_result() ->
  ?stub( gun, await_body, 2, <<"{\"bearer_token\":\"btoken1\",\"account_alias\":\"alias1\"}">> ),

  {reply, _, ResultState } = clc_v2_http_client:handle_call( {get, resource1}, ignored, #{ gun_pid => self() } ),
  ?assertMatch( #{ account_alias := <<"alias1">>, bearer_token := <<"btoken1">> }, ResultState ),

  ?called( gun, post, [ self(), <<"v2/authentication/login">>, [], ?any ] ).



%handle_call_when_no_auth_info_in_state_calls_auth_api() ->
  %meck(gun, ),


