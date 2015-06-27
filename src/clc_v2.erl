-module( clc_v2 ).
-export( [datacenters/0] ).

datacenters() ->
  clc_v2_datacenters:get().
