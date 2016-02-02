-module(clc_v2_custom_fields).

-export([get/1]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> list().
get( Ref ) ->
  clc_v2_http_client:get( Ref, ["accounts", account_alias, "customFields"] ).

