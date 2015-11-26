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
  Alias = element(1, cowboy_req:binding(alias, Req)),
  Id = element(1, cowboy_req:binding(id, Req)),
  {Success, ResponseBody} = get_capabilities(Alias, Id),
  Status = case Success of
    ok -> 200;
    _ -> 403
  end,

  {ok, Response} = cowboy_req:reply(Status, [], jiffy:encode(ResponseBody), Req),
  {ok, Response, State}.

get_capabilities(?ALIAS, Id) ->
  Datacenters = data_server:get(datacenter_capabilities, Id),
  {ok,  Datacenters};
get_capabilities(_, _) ->
  {error, #{ <<"message">> => <<"You do not have permission to access this account.">> }}.
