-module( clc_v2_autoscale_policies ).
-include("macros.hrl").

-export([
  get/1,
  get/2,
  get_server_policy/2,
  update_server_policy/3,
  remove_server_policy/2
  ]).

-define( SERVER_ROUTE( ServerId ),
  ["servers", account_alias, binary_to_list(ServerId), "cpuAutoscalePolicy" ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  clc_v2_http_client:get( AuthRef, ["autoscalePolicies", account_alias] ).

-spec get( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> map().
get( AuthRef, Id ) ->
  clc_v2_http_client:get( AuthRef, ["autoscalePolicies", account_alias, binary_to_list(Id) ] ).

-spec get_server_policy( AuthRef::clc_v2_auth:auth_ref(), ServerId::binary() ) -> map().
get_server_policy( AuthRef, ServerId ) ->
  clc_v2_http_client:get( AuthRef, ?SERVER_ROUTE( ServerId ) ).

-spec update_server_policy( AuthRef::clc_v2_auth:auth_ref(), ServerId::binary(), PolicyId::binary() ) -> ok.
update_server_policy( AuthRef, ServerId, PolicyId ) ->
  Payload = #{ id => PolicyId },
  Response = clc_v2_http_client:put( AuthRef, ?SERVER_ROUTE( ServerId ), Payload ),
  ?OK_OR_ERROR( Response ).

-spec remove_server_policy( AuthRef::clc_v2_auth:auth_ref(), ServerId::binary() ) -> ok.
remove_server_policy( AuthRef, ServerId ) ->
  Response = clc_v2_http_client:delete( AuthRef, ?SERVER_ROUTE( ServerId ) ),
  ?OK_OR_ERROR( Response ).
