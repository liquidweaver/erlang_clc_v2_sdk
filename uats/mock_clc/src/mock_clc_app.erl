-include("data.hrl").
-module(mock_clc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  gen_server:start(?DATASERVER, data_server, [], []),
  Dispatch = cowboy_router:compile(route_matchers()),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8000}], [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
  ok.

route_matchers() ->

  [ {'_',
     [
      { "/v2/authentication/login",
          auth_handler, [] },
      { io_lib:format("/v2/alertPolicies/~s/[:id]", [?ALIAS]),
          alertpolicy_handler, [] },
      { io_lib:format("/v2/antiAffinityPolicies/~s/[:id]", [?ALIAS]),
          antiaffinitypolicy_handler, [] },
      { io_lib:format("/v2/autoscalePolicies/~s/[:id]", [?ALIAS]),
          autoscalepolicy_handler, [] },
      { io_lib:format("/v2/datacenters/~s/[:id]", [?ALIAS]),
          datacenter_handler, [] },
      { io_lib:format("/v2/datacenters/~s/:id/deploymentCapabilities", [?ALIAS]),
          dc_deployment_capability_handler, [] },
      { io_lib:format("/v2/datacenters/~s/:id/bareMetalCapabilities", [?ALIAS]),
          dc_baremetal_capability_handler, [] },
      { io_lib:format("/v2/servers/~s/:server_id/cpuAutoscalePolicy", [?ALIAS]),
          server_autoscalepolicy_handler, [] }
     ]
    } ].
