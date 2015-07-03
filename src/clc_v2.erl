-module( clc_v2 ).
-export( [
  login/2,
  datacenters/1,
  datacenter/2
  ] ).

-spec login( Username::binary(), Password::binary() ) -> clc_v2_auth:auth_ref().
login( Username, Password ) ->
  { ok, AuthRef } = clc_v2_auth_sup:create_worker( Username, Password ),
  AuthRef.

-spec datacenters(Ref::clc_v2_auth:auth_ref()) -> map().
datacenters(Ref) ->
  clc_v2_datacenters:get(Ref).

-spec datacenter(Ref::clc_v2_auth:auth_ref(), Datacenter::binary()) -> map().
datacenter(Ref, Datacenter) ->
  clc_v2_datacenters:get(Ref, Datacenter).
