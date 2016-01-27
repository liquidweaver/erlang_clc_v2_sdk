-module( clc_v2_alert_policies ).
-include("macros.hrl").

-export([
  get/1,
  get/2,
  create/2,
  update/3,
  delete/2
  ]).

-define( ROUTE,
         fun() -> ["alertPolicies", account_alias ] end ).

-define( ID_ROUTE,
         fun( Id ) -> ["alertPolicies", account_alias, binary_to_list(Id) ] end ).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  clc_v2_http_client:get( AuthRef, ?ROUTE() ).

-spec get( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> map().
get( AuthRef, Id ) ->
  clc_v2_http_client:get( AuthRef, ?ID_ROUTE( Id ) ).

-spec create( AuthRef::clc_v2_auth:auth_ref(), Spec::map() ) -> binary() | { error, term() }.
create( AuthRef, Spec ) ->
  Spec1 = to_api_spec(Spec),
  Response = clc_v2_http_client:post( AuthRef, ?ROUTE(), Spec1 ),
  ?ID_OR_ERROR( Response ).

-spec update( AuthRef::clc_v2_auth:auth_ref(), Spec::map(), Id::binary() ) -> ok.
update( AuthRef, Spec, Id ) ->
  Spec1 = to_api_spec(Spec),
  Response = clc_v2_http_client:put( AuthRef, ?ID_ROUTE( Id ), Spec1 ),
  ?OK_OR_ERROR( Response ).

-spec delete( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> ok.
delete( AuthRef, Id ) ->
  Response = clc_v2_http_client:delete( AuthRef, ?ID_ROUTE( Id ) ),
  ?OK_OR_ERROR( Response ).

to_api_spec( #{ name := Name, email_recipients := Recipients, triggers := Triggers } ) ->
  #{ name => Name,
     actions => [ #{ action => email,
                     settings => #{ recipients => Recipients } } ],
     triggers => Triggers
   }.
