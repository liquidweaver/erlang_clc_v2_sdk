-module( clc_v2_datacenters ).

-export([
  get/1
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  clc_v2_http_client:get( AuthRef, ["datacenters", account_alias] ).
