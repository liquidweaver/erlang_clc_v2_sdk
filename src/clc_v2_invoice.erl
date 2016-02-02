-module(clc_v2_invoice).

-export([get/3]).

-spec get( AuthRef::clc_v2_auth:auth_ref(), Year::integer(), Month::integer() ) -> map().
get( Ref, Year, Month ) ->
  clc_v2_http_client:get( Ref, ["invoice", account_alias, integer_to_list(Year), integer_to_list(Month)] ).
