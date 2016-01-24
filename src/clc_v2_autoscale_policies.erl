-module( clc_v2_autoscale_policies ).

-export([
  get/1,
  get/2,
  get_server_policy/2,
  update_server_policy/3,
  remove_server_policy/2
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  clc_v2_http_client:get( AuthRef, ["autoscalePolicies", account_alias] ).

-spec get( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> map().
get( AuthRef, Id ) ->
  clc_v2_http_client:get( AuthRef, ["autoscalePolicies", account_alias, binary_to_list(Id) ] ).

-spec get_server_policy( AuthRef::clc_v2_auth:auth_ref(), ServerId::binary() ) -> map().
get_server_policy( AuthRef, ServerId ) ->
  clc_v2_http_client:get( AuthRef, server_policy_route(ServerId) ).

-spec update_server_policy( AuthRef::clc_v2_auth:auth_ref(), ServerId::binary(), PolicyId::binary() ) -> ok.
update_server_policy( AuthRef, ServerId, PolicyId ) ->
  Payload = #{ id => PolicyId },
  Response = clc_v2_http_client:put( AuthRef, server_policy_route(ServerId), Payload ),
  case Response of
    { ok, _ } -> ok;
    Error -> Error
  end.

-spec remove_server_policy( AuthRef::clc_v2_auth:auth_ref(), ServerId::binary() ) -> ok.
remove_server_policy( AuthRef, ServerId ) ->
  Response = clc_v2_http_client:delete( AuthRef, server_policy_route(ServerId) ),
  case Response of
    { ok, _ } -> ok;
    Error -> Error
  end.

server_policy_route(ServerId) ->
  ["servers", account_alias, binary_to_list(ServerId), "cpuAutoscalePolicy" ].
