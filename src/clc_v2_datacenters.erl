-module( clc_v2_datacenters ).

-export([
  get/1,
  get/2,
  capabilities/2
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  { ok, Datacenters } = clc_v2_http_client:get( AuthRef, ["datacenters", account_alias] ),
  Datacenters.

-spec get( AuthRef::clc_v2_auth:auth_ref(), DatacenterId::binary() ) -> map().
get( AuthRef, DatacenterId ) ->
  { ok, Datacenter } = clc_v2_http_client:get( AuthRef, ["datacenters", account_alias, binary_to_list( DatacenterId )]),
  Datacenter.

-spec capabilities( AuthRef::clc_v2_auth:auth_ref(), DatacenterId::binary() ) -> map().
capabilities( AuthRef, DatacenterId ) ->
  { ok, Datacenter } = clc_v2_http_client:get( AuthRef, ["datacenters", account_alias, binary_to_list( DatacenterId ), "deploymentCapabilities"]),
  Datacenter.
