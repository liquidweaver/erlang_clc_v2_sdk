-module( clc_v2_authentication ).

-export([
  login/2,
  account_alias/1,
  bearer_token/1
  ]).

-spec login( Username::string(), Password::string() ) -> map().
login( Username, Password ) ->
  Body = #{username => Username, password => Password},
  clc_v2_http_client:post( undefined, ["authentication", "login"], Body ).

-spec account_alias( UserInfo::clc_v2_auth:user_info() ) -> string().
account_alias( #{ <<"accountAlias">> := Alias } ) ->
  binary_to_list(Alias).

-spec bearer_token( UserInfo::clc_v2_auth:user_info() ) -> string().
bearer_token( #{ <<"bearerToken">> := Token } ) ->
  binary_to_list(Token).
