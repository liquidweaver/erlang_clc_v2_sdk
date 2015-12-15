-module( clc_v2 ).
-export( [
  login/2,
  datacenters/1,
  datacenter/2,
  datacenter_capabilities/2,
  alert_policies/1,
  alert_policy/2,
  create_alert_policy/2,
  update_alert_policy/3,
  delete_alert_policy/2
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

-spec alert_policy(Ref::clc_v2_auth:auth_ref(), Id::binary()) -> map().
alert_policy(Ref, Id) ->
   clc_v2_alert_policies:get(Ref, Id).

-spec create_alert_policy(Ref::clc_v2_auth:auth_ref(), Spec::map) -> binary().
create_alert_policy(Ref, Spec) ->
   clc_v2_alert_policies:create(Ref, Spec).

-spec update_alert_policy(Ref::clc_v2_auth:auth_ref(), Spec::map, Id::binary()) -> binary().
update_alert_policy(Ref, Spec, Id) ->
   clc_v2_alert_policies:update(Ref, Spec, Id).

-spec delete_alert_policy(Ref::clc_v2_auth:auth_ref(), Id::binary()) -> ok.
delete_alert_policy(Ref, Id) ->
   ok.
