-module( clc_v2 ).
-export( [
  login/2,
  invoice_data/3,
  datacenters/1,
  datacenter/2,
  datacenter_deployment_capabilities/2,
  datacenter_baremetal_capabilities/2,
  alert_policies/1,
  alert_policy/2,
  create_alert_policy/2,
  update_alert_policy/3,
  delete_alert_policy/2,
  antiaffinity_policies/1,
  antiaffinity_policy/2,
  create_antiaffinity_policy/2,
  update_antiaffinity_policy/3,
  delete_antiaffinity_policy/2,
  autoscale_policies/1,
  autoscale_policy/2,
  server_autoscale_policy/2,
  update_server_autoscale_policy/3,
  remove_server_autoscale_policy/2
  ] ).

-spec login( Username::binary(), Password::binary() ) -> clc_v2_auth:auth_ref().
login( Username, Password ) ->
  clc_v2_auth_sup:create_worker( Username, Password ).

-spec invoice_data(Ref::clc_v2_auth:auth_ref(), Year::integer(), Month::integer()) -> map().
invoice_data(Ref, Year, Month) ->
  clc_v2_invoice:get(Ref, Year, Month).

-spec datacenters(Ref::clc_v2_auth:auth_ref()) -> map().
datacenters(Ref) ->
  clc_v2_datacenters:get(Ref).

-spec datacenter(Ref::clc_v2_auth:auth_ref(), Datacenter::binary()) -> map().
datacenter(Ref, Datacenter) ->
  clc_v2_datacenters:get(Ref, Datacenter).

-spec datacenter_deployment_capabilities(Ref::clc_v2_auth:auth_ref(), Datacenter::binary()) -> map().
datacenter_deployment_capabilities(Ref, Datacenter) ->
  clc_v2_datacenters:deployment_capabilities(Ref, Datacenter).

-spec datacenter_baremetal_capabilities(Ref::clc_v2_auth:auth_ref(), Datacenter::binary()) -> map().
datacenter_baremetal_capabilities(Ref, Datacenter) ->
  clc_v2_datacenters:baremetal_capabilities(Ref, Datacenter).

-spec alert_policies(Ref::clc_v2_auth:auth_ref()) -> map().
alert_policies(Ref) ->
   clc_v2_alert_policies:get(Ref).

-spec alert_policy(Ref::clc_v2_auth:auth_ref(), Id::binary()) -> map().
alert_policy(Ref, Id) ->
   clc_v2_alert_policies:get(Ref, Id).

-spec create_alert_policy(Ref::clc_v2_auth:auth_ref(), Spec::map) -> binary().
create_alert_policy(Ref, Spec) ->
   clc_v2_alert_policies:create(Ref, Spec).

-spec update_alert_policy(Ref::clc_v2_auth:auth_ref(), Spec::map, Id::binary()) -> ok.
update_alert_policy(Ref, Spec, Id) ->
   clc_v2_alert_policies:update(Ref, Spec, Id).

-spec delete_alert_policy(Ref::clc_v2_auth:auth_ref(), Id::binary()) -> ok.
delete_alert_policy(Ref, Id) ->
   clc_v2_alert_policies:delete(Ref, Id).

-spec antiaffinity_policies(Ref::clc_v2_auth:auth_ref()) -> map().
antiaffinity_policies(Ref) ->
   clc_v2_antiaffinity_policies:get(Ref).

-spec antiaffinity_policy(Ref::clc_v2_auth:auth_ref(), Id::binary()) -> map().
antiaffinity_policy(Ref, Id) ->
   clc_v2_antiaffinity_policies:get(Ref, Id).

-spec create_antiaffinity_policy(Ref::clc_v2_auth:auth_ref(), Spec::map) -> binary().
create_antiaffinity_policy(Ref, Spec) ->
   clc_v2_antiaffinity_policies:create(Ref, Spec).

-spec update_antiaffinity_policy(Ref::clc_v2_auth:auth_ref(), Spec::map, Id::binary()) -> ok.
update_antiaffinity_policy(Ref, Spec, Id) ->
   clc_v2_antiaffinity_policies:update(Ref, Spec, Id).

-spec delete_antiaffinity_policy(Ref::clc_v2_auth:auth_ref(), Id::binary()) -> ok.
delete_antiaffinity_policy(Ref, Id) ->
   clc_v2_antiaffinity_policies:delete(Ref, Id).

-spec autoscale_policies(Ref::clc_v2_auth:auth_ref()) -> map().
autoscale_policies(Ref) ->
	clc_v2_autoscale_policies:get(Ref).

-spec autoscale_policy(Ref::clc_v2_auth:auth_ref(), Id::binary()) -> map().
autoscale_policy(Ref, Id) ->
	clc_v2_autoscale_policies:get(Ref, Id).

-spec server_autoscale_policy(Ref::clc_v2_auth:auth_ref(), ServerId::binary()) -> map().
server_autoscale_policy(Ref, ServerId) ->
	clc_v2_autoscale_policies:get_server_policy(Ref, ServerId).

-spec update_server_autoscale_policy(Ref::clc_v2_auth:auth_ref(), ServerId::binary(), PolicyId::binary()) -> ok.
update_server_autoscale_policy(Ref, ServerId, PolicyId) ->
	clc_v2_autoscale_policies:update_server_policy(Ref, ServerId, PolicyId).

-spec remove_server_autoscale_policy(Ref::clc_v2_auth:auth_ref(), ServerId::binary()) -> ok.
remove_server_autoscale_policy(Ref, ServerId) ->
	clc_v2_autoscale_policies:remove_server_policy(Ref, ServerId).
