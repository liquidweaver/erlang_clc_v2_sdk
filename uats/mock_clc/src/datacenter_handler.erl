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
  Alias = element(1, cowboy_req:binding(alias, Req)),
  Id = element(1, cowboy_req:binding(id, Req)),
  {Success, ResponseBody} = get_datacenters(Alias, Id),
  Status = case Success of
    ok -> 200;
    _ -> 403
  end,

  {ok, Response} = cowboy_req:reply(Status, [], jiffy:encode(ResponseBody), Req),
  {ok, Response, State}.

get_datacenters(?ALIAS, undefined) ->
  Datacenters = data_server:get(datacenters),
  {ok,  Datacenters};
get_datacenters(?ALIAS, Id) ->
  Datacenters = data_server:get(datacenters, Id),
  {ok,  Datacenters};
get_datacenters(_, _) ->
  {error, #{ <<"message">> => <<"You do not have permission to access this account.">> }}.
