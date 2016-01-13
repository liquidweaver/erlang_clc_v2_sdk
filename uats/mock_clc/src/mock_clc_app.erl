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
      { io_lib:format("/v2/datacenters/~s/[:id]", [?ALIAS]),
          datacenter_handler, [] },
      { io_lib:format("/v2/datacenters/~s/:id/deploymentCapabilities", [?ALIAS]),
          dc_capability_handler, [] }
     ]
    } ].
