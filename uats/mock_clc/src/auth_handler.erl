-include("data.hrl").
-module(auth_handler).
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         post/2]).

init(_ReqType, _Req, _Options) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, undefined_state}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, post}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, post}], Req, State}.

post(Req, State) ->
  {ok, Body, _} = cowboy_req:body(Req),
  Body1 = jiffy:decode(Body, [return_maps]),

  {Success, ResponseBody} = authorize(Body1),
  Status = case Success of
    ok -> 200;
    _ -> 400
  end,

  {ok, Response} = cowboy_req:reply(Status, [], jiffy:encode(ResponseBody), Req),
  {ok, Response, State}.

authorize(#{ <<"username">> := <<"mock_user">>, <<"password">> := <<"mock_password">> }) ->
  {ok, #{ <<"accountAlias">> => list_to_binary(?ALIAS),
          <<"locationAlias">> => <<"Region1">>,
          <<"userName">> => <<"mock_user.mock">>,
          <<"roles">> => [<<"AccountAdmin">>],
          <<"bearerToken">> => auth_helper:token()
        }
  };
authorize(_) ->
  {error, #{ <<"message">> => <<"We didn't recognize the username or password you entered. Please try again.">> }}.

