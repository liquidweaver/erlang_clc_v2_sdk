-module( clc_v2 ).
-export( [
  login/2,
  datacenters/1,
  datacenter/2,
  datacenter_capabilities/2,
  alert_policies/1
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

-spec datacenter_capabilities(Ref::clc_v2_auth:auth_ref(), Datacenter::binary()) -> map().
datacenter_capabilities(Ref, Datacenter) ->
  clc_v2_datacenters:capabilities(Ref, Datacenter).

-spec alert_policies(Ref::clc_v2_auth:auth_ref()) -> map().
alert_policies(Ref) ->
   clc_v2_alert_policies:get(Ref).
