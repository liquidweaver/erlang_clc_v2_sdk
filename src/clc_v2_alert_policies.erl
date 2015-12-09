-module( clc_v2_alert_policies ).

-export([
  get/1,
  get/2
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  {ok, Policies}  = clc_v2_http_client:get( AuthRef, ["alertPolicies", account_alias ] ),
  Policies.

-spec get( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> map().
get( AuthRef, Id ) ->
  {ok, Policy } = clc_v2_http_client:get( AuthRef, ["alertPolicies", account_alias, binary_to_list(Id) ] ),
  Policy.
