-include("data.hrl").
-module(dc_capability_handler).

-export([init/3, terminate/3, handle/2]).

init(_Type, Req, _Options) ->
  {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  handle_method(Method, Req1, State).

handle_method(<<"GET">>, Req, State) ->
  Id = element(1, cowboy_req:binding(id, Req)),
  ResponseBody = data_server:get(datacenter_capabilities, Id),
  {ok, Response} = cowboy_req:reply(200, [], jiffy:encode(ResponseBody), Req),
  {ok, Response, State}.
