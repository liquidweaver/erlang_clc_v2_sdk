-include("data.hrl").
-module(auth_handler).
-export([init/3, terminate/3, handle/2]).

init(_Type, Req, _Options) ->
  {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  handle_method(Method, Req1, State).

handle_method(<<"POST">>, Req, State) ->
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
  {ok, #{ <<"accountAlias">> => ?ALIAS,
          <<"locationAlias">> => <<"Region1">>,
          <<"userName">> => <<"mock_user.mock">>,
          <<"roles">> => [<<"AccountAdmin">>],
          <<"bearerToken">> => <<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1cm46YXBpLXRpZXIzIiwiYXVkIjoidXJuOnRpZXIzLXVzZXJzIiwibmJmIjoxNDM2MDI5ODc4LCJleHAiOjE0MzcyMzk0NzgsInJvbGUiOiJBY2NvdW50QWRtaW4iLCJ1bmlxdWVfbmFtZSI6ImRzY2h3YWJlLnQzYmsiLCJ1cm46dGllcjM6YWNjb3VudC1hbGlhcyI6IlQzQksiLCJ1cm46dGllcjM6bG9jYXRpb24tYWxpYXMiOiJWQTEifQ.K9_YevPF_2iFnOI-2_nht8YRm_4CY1OFk8eMlXI1IgA">>
        }
  };
authorize(_) ->
  {error, #{ <<"message">> => <<"We didn't recognize the username or password you entered. Please try again.">> }}.

