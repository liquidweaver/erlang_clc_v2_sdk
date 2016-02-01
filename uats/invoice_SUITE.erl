-module(invoice_SUITE).
-include("uat_helper.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([clc_v2_invoice_data_returns_invoice_data/1]).

all() -> [clc_v2_invoice_data_returns_invoice_data].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

clc_v2_invoice_data_returns_invoice_data(Config) ->
  Expected = random_invoice(),
  InvoiceYear = 2016,
  InvoiceMonth = 12,
	Key = <<(integer_to_binary(InvoiceYear))/binary, (integer_to_binary(InvoiceMonth))/binary>>,
  data_server:put(invoices, Key, Expected),

  { ok, Actual } = clc_v2:invoice_data(proplists:get_value( auth_ref, Config ), InvoiceYear, InvoiceMonth),

  assert:equal(Expected, Actual),
  ok.

random_invoice() ->
	#{
		<<"id">> => <<"ALIAS69849A66">>,
		<<"terms">> => <<"Net 15">>,
		<<"companyName">> => <<"CTL Cloud Solutions">>,
		<<"accountAlias">> => <<"ALIAS">>,
		<<"pricingAccountAlias">> => <<"ALIAS">>,
		<<"parentAccountAlias">> => <<"PALIAS">>,
		<<"address1">> => <<"1100 112th Ave NE">>,
		<<"address2">> => <<"Suite 400">>,
		<<"city">> => <<"Bellevue">>,
		<<"stateProvince">> => <<"WA">>,
		<<"postalCode">> => <<"98004">>,
		<<"billingContactEmail">> => <<"billing@domain.com">>,
		<<"invoiceCCEmail">> => <<"invoice@domain.com">>,
		<<"totalAmount">> => 0,
		<<"invoiceDate">> => <<"2016-12-01T00 =>00 =>00Z">>,
		<<"poNumber">> => <<"12345">>,
		<<"lineItems">> =>
			[ #{
				<<"quantity">> => ?RINT(),
				<<"description">> => <<"Description1">>,
				<<"unitCost">> => ?RFLOAT(),
				<<"itemTotal">> => ?RFLOAT(),
				<<"serviceLocation">> => <<"Location1">>,
				<<"itemDetails">> =>
					[ #{
							<<"description">> => <<"Detail1">>,
							<<"cost">> =>  ?RFLOAT()
					} ]
				},
				#{
					<<"quantity">> => ?RINT(),
					<<"description">> => <<"Item2">>,
					<<"unitCost">> => ?RFLOAT(),
					<<"itemTotal">> => ?RFLOAT(),
					<<"serviceLocation">> => <<"Location2">>,
					<<"itemDetails">> => []
				}
			]
	}.
