-module( clc_v2_authentication ).

-export([
  login/2,
  account_alias/1
  ]).

-spec login( Username::string(), Password::string() ) -> map().
login( Username, Password ) ->
  Body = #{username => Username, password => Password},
  clc_v2_http_client:post( ["authentication", "login"], Body ).

-spec account_alias( UserInfo::clc_v2_auth:user_info() ) -> string().
account_alias( #{ <<"accountAlias">> := Alias } ) ->
  binary_to_list(Alias).
