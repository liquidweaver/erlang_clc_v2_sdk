-module(clc_v2_http_client).

-export([
  get/2
  ]).

-type route_token() :: account_alias.

-spec get( AuthRef::clc_v2_auth:auth_ref(), Route::[string() | route_token()] ) -> map().
get( AuthRef, Route ) ->
  UserInfo = clc_v2_auth:user_info( AuthRef ),
  BearerToken = clc_v2_authorization:bearer_token( UserInfo ),
  Headers = [{"Authorization", "Bearer " ++ BearerToken}],
  { ok, ApiBase } = application:get_env( clc_v2, api_base ),
  Url = build_url(UserInfo, Route, ApiBase),
  ibrowse:send_req( Url, Headers, get).

build_url(UserInfo, [H | Tail], Acc) when is_list(H)->
  build_url(UserInfo, Tail, Acc ++ "/" ++ H);
build_url(UserInfo, [H | Tail], Acc) when is_atom(H)->
  build_url(UserInfo, Tail, Acc ++ "/" ++ clc_v2_authorization:H(UserInfo));
build_url(_UserInfo, [], Acc) ->
  Acc.
