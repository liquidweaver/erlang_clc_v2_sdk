-module( clc_v2_http_client_tests ).
-include( "test_fixture.hrl" ).

setup() ->
  ?meck( [clc_v2_auth, clc_v2_authorization, ibrowse], [non_strict] ),
  ?meck( application, [unstick] ),
  ?stub( clc_v2_auth, user_info, 1, user_info1 ),
  ?stub( clc_v2_authorization, bearer_token, 1, "LONG_BEARER_TOKEN" ),
  ?stub( clc_v2_authorization, route_lens, 1, "lens_result1" ),
  ?stub( ibrowse, send_req, 3, {ok, 200, [], []} ),
  ?stub( application, get_env, fun( clc_v2, api_base ) -> {ok, "http://api.base"};
                                  ( _, _ ) -> {error, doing_it_wrong}
                               end).


get_retrieves_user_info_from_auth_actor_using_auth_ref() ->
  clc_v2_http_client:get( auth_ref1, ["route1"] ),

  ?called( clc_v2_auth, user_info, [auth_ref1] ).

get_calls_ibrowse_with_base_api_url_and_route() ->
  clc_v2_http_client:get( auth_ref1, ["route1"] ),

  ?called( ibrowse, send_req, ["http://api.base/route1", ?any, get] ).

get_calls_ibrowse_with_bearer_token_header() ->
  clc_v2_http_client:get( auth_ref1, ["route1"] ),

  ?called( clc_v2_authorization, bearer_token, [user_info1] ),
  Headers = ?capture( ibrowse, send_req, 3, 2 ),
  ?assert(lists:member( {"Authorization", "Bearer LONG_BEARER_TOKEN"}, Headers )).

get_appends_multiple_route_directories_to_api_base() ->
  clc_v2_http_client:get( auth_ref1, ["route1", "route2", "route3"] ),

  ?called( ibrowse, send_req, ["http://api.base/route1/route2/route3", ?any, get] ).

get_appends_calls_authorization_lens_to_resolve_atom_route_directories() ->
  clc_v2_http_client:get( auth_ref1, ["route1", route_lens, "route3"] ),

  ?called( clc_v2_authorization, route_lens, [user_info1] ),
  ?called( ibrowse, send_req, ["http://api.base/route1/lens_result1/route3", ?any, get] ).
