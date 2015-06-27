-module( clc_v2_auth_tests ).
-include( "test_fixture.hrl" ).

init_with_username_and_password_for_state() ->
  ?assertEqual( {ok, {username1, password1, unset}}, clc_v2_auth:init([username1, password1]) ).

handle_call_user_info_when_no_state_info_delegates_to_authorization() ->
  ?meck( clc_v2_authorization, [non_strict] ),
  ?stub( clc_v2_authorization, login, 2, {ok, userinfo1} ),

  { reply, userinfo1, NewState } = clc_v2_auth:handle_call( user_info, from, {username1, password1, unset} ),
  ?assertEqual( { username1, password1, userinfo1 }, NewState ),

  ?called( clc_v2_authorization, login, [username1, password1] ).
