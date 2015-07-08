-module( clc_v2_alert_policies ).

-export([
  get/1
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  {ok, Alerts } = clc_v2_http_client:get( AuthRef, ["alertPolicies", account_alias ] ),
  Alerts.
