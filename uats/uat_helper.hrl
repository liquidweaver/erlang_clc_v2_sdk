-include_lib("common_test/include/ct.hrl").

-define( V2_API_USERNAME, "UNSET" ).
-define( V2_API_PASSWORD, "UNSET" ).
-define( START_APP_AND_GET_AUTH_REF(Config),
  {ok, _Pid} = application:ensure_all_started( clc_v2 ),
  AuthRef = clc_v2:login( <<?V2_API_USERNAME>>, <<?V2_API_PASSWORD>> ),
  [{ auth_ref, AuthRef } | Config ] ).
