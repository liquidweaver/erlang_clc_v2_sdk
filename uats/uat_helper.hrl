-include_lib("common_test/include/ct.hrl").

-define( START_APP_AND_GET_AUTH_REF(Config),
  {ok, _} = application:ensure_all_started( clc_v2 ),
  ok = application:set_env(clc_v2, api_base, "http://localhost:8000/v2"),
  {ok, _} = application:ensure_all_started( mock_clc, temporary ),
  AuthRef = clc_v2:login( <<"mock_user">>, <<"mock_password">>),
  [{ auth_ref, AuthRef } | Config ] ).
-define( TEARDOWN(),
  application:stop( clc_v2 ),
  cowboy:stop_listener(http) ).

-define( ITEMS(Value), #{ <<"items">> => Value }).
-define( RANDOMS(),
  begin
    Fun = fun() ->
            {_, _, Milliseconds} = os:timestamp(),
            random:seed(Milliseconds),
            Float = trunc(random:uniform() * 1000)/10,
            {Float, integer_to_binary(trunc(Float))}
          end,
    Fun()
  end).
