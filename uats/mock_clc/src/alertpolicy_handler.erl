-include("data.hrl").
-module(alertpolicy_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         is_authorized/2,
         forbidden/2,
         unsupported/2,
         get/2,
         create/2]).

init(_ReqType, _Req, _Options) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, undefined_state}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"*">>, unsupported},
    {<<"application/json">>, get}
   ],Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"*">>, unsupported},
    {<<"application/json">>, create}
   ],Req, State}.

is_authorized(Req, State) ->
	case auth_helper:is_authenticated(Req) of
		false -> {{false,<<"Bearer">>}, Req, State};
		true -> {true, Req, State}
	end.

forbidden(Req, State) ->
	{(not auth_helper:is_authorized(Req)), Req, State}.

unsupported(Req, State) ->
  Response = cowboy_req:reply(400, [], <<"unsupported content type">>, Req),
  {ok, Response, State}.

get(Req, State) ->
  Id = element(1, cowboy_req:binding(id, Req)),
  Response = get_policies(Id),
  {jiffy:encode(Response), Req, State}.

create(Req, State) ->
  {ok, Body, _} = cowboy_req:body(Req),
  Spec = jiffy:decode(Body, [return_maps]),

  Id = integer_to_binary(element(3, now())),
  data_server:put(alert_policies, Id, Spec),

  {ok, Response} = cowboy_req:reply(200, [], jiffy:encode(#{<<"id">> => Id}), Req),
  {ok, Response, State}.

get_policies(undefined) ->
  data_server:get(alert_policies);
get_policies(Id) ->
  data_server:get(alert_policies, Id).

