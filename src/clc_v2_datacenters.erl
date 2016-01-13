-module( clc_v2_datacenters ).

-export([
  get/1,
  get/2,
  deployment_capabilities/2
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  clc_v2_http_client:get( AuthRef, ["datacenters", account_alias] ).

-spec get( AuthRef::clc_v2_auth:auth_ref(), DatacenterId::binary() ) -> map().
get( AuthRef, DatacenterId ) ->
  clc_v2_http_client:get( AuthRef, ["datacenters", account_alias, binary_to_list( DatacenterId )]).

-spec deployment_capabilities( AuthRef::clc_v2_auth:auth_ref(), DatacenterId::binary() ) -> map().
deployment_capabilities( AuthRef, DatacenterId ) ->
  clc_v2_http_client:get( AuthRef, ["datacenters", account_alias, binary_to_list( DatacenterId ), "deploymentCapabilities"]).
