-module(mock_clc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile(route_matchers()),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8000}], [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
  ok.

route_matchers() ->
  [ {'_',
     [ { "/authentication/login", auth_handler, [] }]
    } ].
