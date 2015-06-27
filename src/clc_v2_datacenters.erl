-module( clc_v2_datacenters ).

-export([
  get/0
  ]).

get() ->
  clc_v2_http_client:get([<<"datacenters">>, account_alias]).
