-include("data.hrl").
-module(alertpolicy_handler).

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
  {Success, ResponseBody} = get_alert_policies(Alias),
  Status = case Success of
    ok -> 200;
    _ -> 403
  end,

  {ok, Response} = cowboy_req:reply(Status, [], jiffy:encode(ResponseBody), Req),
  {ok, Response, State}.

get_alert_policies(?ALIAS) ->
  AlertPolicies = data_server:get(alert_policies),
  {ok,  AlertPolicies};
get_alert_policies(_) ->
  {error, #{ <<"message">> => <<"You do not have permission to access this account.">> }}.
