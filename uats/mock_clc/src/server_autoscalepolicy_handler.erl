-include("data.hrl").
-module(server_autoscalepolicy_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         is_authorized/2,
         forbidden/2,
         unsupported/2,
         read/2,
         write/2,
         delete_resource/2]).

init(_ReqType, _Req, _Options) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, undefined_state}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"*">>, unsupported},
    {<<"application/json">>, read}
   ],Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"*">>, unsupported},
    {<<"application/json">>, write}
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

read(Req, State) ->
  ServerId = element(1, cowboy_req:binding(server_id, Req)),
  Response = data_server:get(server_autoscale_policies, ServerId),
  {jiffy:encode(Response), Req, State}.

write(Req, State) ->
  ServerId = element(1, cowboy_req:binding(server_id, Req)),
  {ok, Body, _} = cowboy_req:body(Req),
  Spec = #{<<"id">> := PolicyId} = jiffy:decode(Body, [return_maps]),

  data_server:put(server_autoscale_policies, ServerId, Spec),

  {ok, Response} = cowboy_req:reply(200, [], jiffy:encode(#{<<"id">> => PolicyId}), Req),
  {ok, Response, State}.

delete_resource(Req, State) ->
  ServerId = element(1, cowboy_req:binding(server_id, Req)),

  data_server:put(server_autoscale_policies, ServerId, deleted),

  {true, Req, State}.
