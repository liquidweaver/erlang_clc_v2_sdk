-module(custom_fields_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_custom_fields_returns_custom_fields/1]).

all() -> [clc_v2_custom_fields_returns_custom_fields].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

clc_v2_custom_fields_returns_custom_fields(Config) ->
  Expected = random_fields(),
  data_server:put(custom_fields, Expected),

  { ok, Actual } = clc_v2:custom_fields(?AUTH(Config)),

  assert:equal(Expected, Actual),
  ok.

random_fields() ->
	[
		#{
			<<"id">> => ?RBIN(),
			<<"name">> => <<"Production">>,
			<<"isRequired">> => true,
			<<"type">> => <<"checkbox">>,
			<<"options">> => []
		},
		#{
			<<"id">> => ?RBIN(),
			<<"name">> => <<"Color">>,
			<<"isRequired">> => true,
			<<"type">> => <<"option">>,
			<<"options">> => [
				#{
				<<"name">> => <<"red">>,
				<<"value">> => <<"Red">>
				},
				#{
				<<"name">> => <<"blue">>,
				<<"value">> => <<"Blue">>
				}
			]
		}
	].
