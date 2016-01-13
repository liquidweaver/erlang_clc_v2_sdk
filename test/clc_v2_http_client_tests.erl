-module( clc_v2_http_client_tests ).
-include( "test_fixture.hrl" ).

setup() ->
  ?meck( [clc_v2_auth, clc_v2_authentication, ibrowse], [non_strict] ),
  ?meck( application, [unstick] ),
  ?stub( clc_v2_auth, user_info, 1, user_info1 ),
  ?stub( clc_v2_authentication, bearer_token, 1, "LONG_BEARER_TOKEN" ),
  ?stub( clc_v2_authentication, route_lens, 1, "lens_result1" ),
  ?stub( ibrowse, send_req, 4, {ok, "200", [], <<"{}">>} ),
  ?stub( application, get_env, fun( clc_v2, api_base ) -> {ok, "http://api.base"};
                                  ( _, _ ) -> {error, doing_it_wrong}
                               end).


get_retrieves_user_info_from_auth_actor_using_auth_ref() ->
  clc_v2_http_client:get( auth_ref1, ["route1"] ),

  ?called( clc_v2_auth, user_info, [auth_ref1] ).

get_calls_ibrowse_with_base_api_url_and_route() ->
  clc_v2_http_client:get( auth_ref1, ["route1"] ),

  ?called( ibrowse, send_req, ["http://api.base/route1", ?any, get, []] ).

get_calls_ibrowse_with_accept_json_header() ->
  clc_v2_http_client:get( auth_ref1, ["route1"] ),

  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assert(lists:member( {"Accept", "application/json"}, Headers )).

get_calls_ibrowse_with_bearer_token_header() ->
  clc_v2_http_client:get( auth_ref1, ["route1"] ),

  ?called( clc_v2_authentication, bearer_token, [user_info1] ),
  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assert(lists:member( {"Authorization", "Bearer LONG_BEARER_TOKEN"}, Headers )).

get_appends_multiple_route_directories_to_api_base() ->
  clc_v2_http_client:get( auth_ref1, ["route1", "route2", "route3"] ),

  ?called( ibrowse, send_req, ["http://api.base/route1/route2/route3", ?any, get, []] ).

get_calls_authentication_lens_to_resolve_atom_route_directories() ->
  clc_v2_http_client:get( auth_ref1, ["route1", route_lens, "route3"] ),

  ?called( clc_v2_authentication, route_lens, [user_info1] ),
  ?called( ibrowse, send_req, ["http://api.base/route1/lens_result1/route3", ?any, get, []] ).

get_decodes_response_body() ->
  ResponseBody = <<"{\"key1\":\"value1\"}">>,
  ?stub( ibrowse, send_req, 4, {ok, "200", [], ResponseBody }),

  ?assertMatch({ ok, #{ <<"key1">> := <<"value1">> }}, clc_v2_http_client:get( auth_ref1, ["route1"])).

get_returns_error_on_not2xx() ->
  ?stub( ibrowse, send_req, 4, {ok, "not2xx", [], <<>> }),

  ?assertMatch({ error, _ }, clc_v2_http_client:get( auth_ref1, ["route1"])).

post_appends_multiple_route_directories_to_api_base_and_posts() ->
  clc_v2_http_client:post(  auth_ref1, ["route1", "route2"], #{ key1 => "value1", key2 => "value2" }),

  ?called( ibrowse, send_req, ["http://api.base/route1/route2", ?any, post, ?any] ).

post_calls_authentication_lens_to_resolve_atom_route_directories() ->
  clc_v2_http_client:post( auth_ref1, ["route1", route_lens, "route3"], #{} ),

  ?called( clc_v2_authentication, route_lens, [user_info1] ),
  ?called( ibrowse, send_req, ["http://api.base/route1/lens_result1/route3", ?any, post, ?any ] ).

post_encodes_body_as_json() ->
  clc_v2_http_client:post( auth_ref1, ["route1"], #{ key1 => <<"value1">>, key2 => <<"value2">> }),
  ExpectedJson = <<"{\"key2\":\"value2\",\"key1\":\"value1\"}">>,

  ?called( ibrowse, send_req, [?any, ?any, ?any, ExpectedJson] ).

post_sends_authorization_header_when_auth_ref_supplied() ->
  clc_v2_http_client:post( auth_ref1, ["route1"], #{} ),

  ?called( clc_v2_authentication, bearer_token, [user_info1] ),
  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assert(lists:member( {"Authorization", "Bearer LONG_BEARER_TOKEN"}, Headers )).

post_omits_authorization_header_when_auth_ref_undefined() ->
  clc_v2_http_client:post( undefined, ["route1"], #{} ),

  ?not_called( clc_v2_authentication, bearer_token, [user_info1] ),
  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assertNot(lists:member( {"Authorization", "Bearer LONG_BEARER_TOKEN"}, Headers )).

post_sends_accept_header() ->
  clc_v2_http_client:post( auth_ref1, ["route1"], #{} ),

  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assert(lists:member( {"Accept", "application/json"}, Headers )).

post_sends_content_type_header() ->
  clc_v2_http_client:post(  auth_ref1, ["route1"], #{ key1 => <<"value1">>, key2 => <<"value2">> }),

  Headers = ?capture( ibrowse, send_req, 4, 2),
  ?assert(lists:member( {"Content-Type", "application/json"}, Headers )).

post_decodes_response_body() ->
  ResponseBody = <<"{\"key1\":\"value1\"}">>,
  ?stub( ibrowse, send_req, 4, {ok, "200", [], ResponseBody }),

  ?assertMatch({ok, #{ <<"key1">> := <<"value1">> }}, clc_v2_http_client:post( auth_ref1, ["route1"], #{})).

post_returns_error_on_not2xx() ->
  ?stub( ibrowse, send_req, 4, {ok, "not2xx", [], <<>> }),

  ?assertMatch({ error, _ }, clc_v2_http_client:post( auth_ref1, ["route1"], #{})).

put_appends_multiple_route_directories_to_api_base_and_posts() ->
  clc_v2_http_client:put(  auth_ref1, ["route1", "route2"], #{ key1 => "value1", key2 => "value2" }),

  ?called( ibrowse, send_req, ["http://api.base/route1/route2", ?any, put, ?any] ).

put_calls_authentication_lens_to_resolve_atom_route_directories() ->
  clc_v2_http_client:put( auth_ref1, ["route1", route_lens, "route3"], #{} ),

  ?called( clc_v2_authentication, route_lens, [user_info1] ),
  ?called( ibrowse, send_req, ["http://api.base/route1/lens_result1/route3", ?any, put, ?any ] ).

put_encodes_body_as_json() ->
  clc_v2_http_client:put( auth_ref1, ["route1"], #{ key1 => <<"value1">>, key2 => <<"value2">> }),
  ExpectedJson = <<"{\"key2\":\"value2\",\"key1\":\"value1\"}">>,

  ?called( ibrowse, send_req, [?any, ?any, ?any, ExpectedJson] ).

put_sends_authorization_header() ->
  clc_v2_http_client:put( auth_ref1, ["route1"], #{} ),

  ?called( clc_v2_authentication, bearer_token, [user_info1] ),
  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assert(lists:member( {"Authorization", "Bearer LONG_BEARER_TOKEN"}, Headers )).

put_sends_accept_header() ->
  clc_v2_http_client:put( auth_ref1, ["route1"], #{} ),

  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assert(lists:member( {"Accept", "application/json"}, Headers )).

put_sends_content_type_header() ->
  clc_v2_http_client:put(  auth_ref1, ["route1"], #{ key1 => <<"value1">>, key2 => <<"value2">> }),

  Headers = ?capture( ibrowse, send_req, 4, 2),
  ?assert(lists:member( {"Content-Type", "application/json"}, Headers )).

put_decodes_response_body() ->
  ResponseBody = <<"{\"key1\":\"value1\"}">>,
  ?stub( ibrowse, send_req, 4, {ok, "200", [], ResponseBody }),

  ?assertMatch({ok, #{ <<"key1">> := <<"value1">> }}, clc_v2_http_client:put( auth_ref1, ["route1"], #{})).

put_returns_error_on_not2xx() ->
  ?stub( ibrowse, send_req, 4, {ok, "not2xx", [], <<>> }),

  ?assertMatch({ error, _ }, clc_v2_http_client:put( auth_ref1, ["route1"], #{})).

delete_appends_multiple_route_directories_to_api_base_and_posts() ->
  ?stub( ibrowse, send_req, 4, {ok, "204", [], ignored1} ),

  clc_v2_http_client:delete( auth_ref1, ["route1", "route2"] ),

  ?called( ibrowse, send_req, ["http://api.base/route1/route2", ?any, delete, []] ).

delete_calls_authentication_lens_to_resolve_atom_route_directories() ->
  ?stub( ibrowse, send_req, 4, {ok, "204", [], ignored1} ),

  clc_v2_http_client:delete( auth_ref1, ["route1", route_lens, "route3"] ),

  ?called( clc_v2_authentication, route_lens, [user_info1] ),
  ?called( ibrowse, send_req, ["http://api.base/route1/lens_result1/route3", ?any, delete, [] ] ).

delete_sends_authorization_header() ->
  ?stub( ibrowse, send_req, 4, {ok, "204", [], ignored1} ),

  clc_v2_http_client:delete( auth_ref1, ["route1"] ),

  ?called( clc_v2_authentication, bearer_token, [user_info1] ),
  Headers = ?capture( ibrowse, send_req, 4, 2 ),
  ?assert(lists:member( {"Authorization", "Bearer LONG_BEARER_TOKEN"}, Headers )).

delete_returns_ok_on_success() ->
  ?stub( ibrowse, send_req, 4, {ok, "204", [], ignored1} ),

  ?assertEqual( ok, clc_v2_http_client:delete(auth_ref1, ["route1"]) ).

delete_returns_error_on_not2xx() ->
  ?stub( ibrowse, send_req, 4, {ok, "not2xx", [], <<>> }),

  ?assertMatch({ error, _ }, clc_v2_http_client:delete( auth_ref1, ["route1"])).
