-include("data.hrl").
-module(datacenter_handler).

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
  ResponseBody = get_datacenters(Id),
  {ok, Response} = cowboy_req:reply(200, [], jiffy:encode(ResponseBody), Req),
  {ok, Response, State}.

get_datacenters(undefined) ->
  data_server:get(datacenters);
get_datacenters(Id) ->
  data_server:get(datacenters, Id).
