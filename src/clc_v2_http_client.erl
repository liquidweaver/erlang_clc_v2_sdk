-module(clc_v2_http_client).

-export([
  get/2,
  post/3,
  put/3
  ]).

-define(PAYLOAD_HEADERS(), [ {"Content-Type","application/json"},
                              {"Accept", "application/json"} ] ).
-type route_token() :: account_alias.

-spec get( AuthRef::clc_v2_auth:auth_ref(), Route::[string() | route_token()] ) -> { ok, map() } | { error, term() }.
get( AuthRef, Route ) ->
  Headers = [ {"Accept", "application/json"} ],
  send_req( AuthRef, Route, Headers, get, []).

-spec post( AuthRef::clc_v2_auth:auth_ref(), Route::[string() | route_token()], Body::string() | binary() ) -> { ok, map() } | { error, term() }.
post( AuthRef, Route, Body ) ->
  send_req( AuthRef, Route, ?PAYLOAD_HEADERS(), post, jiffy:encode(Body)).

-spec put( AuthRef::clc_v2_auth:auth_ref(), Route::[string() | route_token()], Body::string() | binary() ) -> { ok, map() } | { error, term() }.
put( AuthRef, Route, Body ) ->
  send_req( AuthRef, Route, ?PAYLOAD_HEADERS(), put, jiffy:encode(Body)).

send_req( undefined, Route, Headers, Method, Body ) ->
  Url = build_url(#{}, Route, api_base()),
  send_req1( Url, Headers, Method, Body );
send_req( AuthRef, Route, Headers, Method, Body ) ->
  UserInfo = clc_v2_auth:user_info( AuthRef ),
  BearerToken = clc_v2_authentication:bearer_token( UserInfo ),
  Headers1 = [ { "Authorization", "Bearer " ++ BearerToken } | Headers ],
  Url = build_url( UserInfo, Route, api_base() ),
  send_req1( Url, Headers1, Method, Body ).

send_req1( Url, Headers, Method, Body ) ->
  {ok, "200", _, ResponseBody} = ibrowse:send_req( Url, Headers, Method, Body ),
  {ok, jiffy:decode( ResponseBody, [return_maps] )}.

build_url(UserInfo, [H | Tail], Acc) when is_list(H)->
  build_url(UserInfo, Tail, Acc ++ "/" ++ H);
build_url(UserInfo, [H | Tail], Acc) when is_atom(H)->
  build_url(UserInfo, Tail, Acc ++ "/" ++ clc_v2_authentication:H(UserInfo));
build_url(_UserInfo, [], Acc) ->
  Acc.

api_base() ->
  { ok, ApiBase } = application:get_env( clc_v2, api_base ),
  ApiBase.
