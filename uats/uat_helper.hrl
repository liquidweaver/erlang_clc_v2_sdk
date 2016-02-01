-include_lib("common_test/include/ct.hrl").

-define( SUITE_SETUP(Config),
  {_, _, Milliseconds} = os:timestamp(),
  random:seed(Milliseconds),
  {ok, _} = application:ensure_all_started( clc_v2 ),
  ok = application:set_env(clc_v2, api_base, "http://localhost:8000/v2"),
  {ok, _} = application:ensure_all_started( mock_clc, temporary ),
  { ok, AuthRef } = clc_v2:login( <<"mock_user">>, <<"mock_password">>),
  [{ auth_ref, AuthRef } | Config ] ).
-define( SUITE_TEARDOWN(),
  application:stop( clc_v2 ),
  cowboy:stop_listener(http) ).
-define( TEST_TEARDOWN(),
  data_server:clear() ).
-define( RFLOAT(), trunc(random:uniform() * 1000)/10 ).
-define( RINT(), random:uniform(100) ).
-define( RBIN(), list_to_binary([random:uniform(26) + 64 || _ <- lists:seq(1,16)]) ).
-define( AUTH(Config), proplists:get_value( auth_ref, Config )).
