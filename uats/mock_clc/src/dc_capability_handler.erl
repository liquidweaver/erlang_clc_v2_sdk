-include("data.hrl").
-module(dc_capability_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         forbidden/2,
         unsupported/2,
         get/2]).

init(_ReqType, _Req, _Options) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, undefined_state}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"*">>, unsupported},
    {<<"application/json">>, get}
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
  ResponseBody = data_server:get(datacenter_deployment_capabilities, Id),
  {ok, Response} = cowboy_req:reply(200, [], jiffy:encode(ResponseBody), Req),
  {ok, Response, State}.
