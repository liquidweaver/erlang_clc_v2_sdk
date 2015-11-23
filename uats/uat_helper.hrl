-include_lib("common_test/include/ct.hrl").

-define( V2_API_USERNAME, "UNSET" ).
-define( V2_API_PASSWORD, "UNSET" ).
-define( START_APP_AND_GET_AUTH_REF(Config),
  {ok, _} = application:ensure_all_started( clc_v2 ),
  {ok, _} = application:ensure_all_started( mock_clc ),
  AuthRef = clc_v2:login( <<?V2_API_USERNAME>>, <<?V2_API_PASSWORD>> ),
  [{ auth_ref, AuthRef } | Config ] ).
-define( TEARDOWN(),
  application:stop( clc_v2 ),
  application:stop( mock_clc ) ).

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
