-module(alert_policies_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element/1]).

all() -> [clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?START_APP_AND_GET_AUTH_REF(Config).

end_per_suite(_Config) ->
  application:stop( clc_v2 ).

clc_v2_alerts_returns_a_list_of_actions_with_at_least_one_element(Config) ->
  AuthRef = proplists:get_value( auth_ref, Config ),
  Alerts = clc_v2:alert_policies(AuthRef),
  io:format( "Alerts: ~p~n", [Alerts] ),
  #{ <<"items">> := Actions } = Alerts,
  true = (length(Actions) >= 1),
  ok.
