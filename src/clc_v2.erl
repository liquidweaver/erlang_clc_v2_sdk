-module( clc_v2 ).
-export( [
  login/2,
  datacenters/0
  ] ).

-spec login( Username::binary(), Password::binary ) -> clc_v2_auth:auth_ref().
login( Username, Password ) ->
  clc_v2_auth_sup:create_worker( Username, Password ).

datacenters() ->
  clc_v2_datacenters:get().
