-module( clc_v2_antiaffinity_policies ).

-export([
  get/1,
  get/2,
  create/2,
  update/3,
  delete/2
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  clc_v2_http_client:get( AuthRef, ["antiAffinityPolicies", account_alias ] ).

-spec get( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> map().
get( AuthRef, Id ) ->
  clc_v2_http_client:get( AuthRef, ["antiAffinityPolicies", account_alias, binary_to_list(Id) ] ).

-spec create( AuthRef::clc_v2_auth:auth_ref(), Spec::map() ) -> binary() | { error, term() }.
create( AuthRef, Spec ) ->
  Response = clc_v2_http_client:post( AuthRef, ["antiAffinityPolicies", account_alias ], Spec ),
  case Response of
    {ok, #{ <<"id">> := Id } } -> { ok, Id };
    Error -> Error
  end.

-spec update( AuthRef::clc_v2_auth:auth_ref(), Spec::map(), Id::binary() ) -> ok.
update( AuthRef, Spec, Id ) ->
  Response = clc_v2_http_client:put( AuthRef, ["antiAffinityPolicies", account_alias, binary_to_list(Id) ], Spec ),
  case Response of
    { ok, _ } -> ok;
    Error -> Error
  end.

-spec delete( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> ok.
delete( AuthRef, Id ) ->
  Response = clc_v2_http_client:delete( AuthRef, ["antiAffinityPolicies", account_alias, binary_to_list(Id) ] ),
  case Response of
    { ok, _ } -> ok;
    Error -> Error
  end.
