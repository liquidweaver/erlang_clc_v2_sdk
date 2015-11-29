-module(auth_helper).

-export([ is_authenticated/1,
          is_authorized/1,
          token/0]).

is_authenticated(Req) ->
	req_token(Req) /= <<>>.

is_authorized(Req) ->
	req_token(Req) =:= token().

token() ->
  <<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1cm46YXBpLXRpZXIzIiwiYXVkIjoidXJuOnRpZXIzLXVzZXJzIiwibmJmIjoxNDM2MDI5ODc4LCJleHAiOjE0MzcyMzk0NzgsInJvbGUiOiJBY2NvdW50QWRtaW4iLCJ1bmlxdWVfbmFtZSI6ImRzY2h3YWJlLnQzYmsiLCJ1cm46dGllcjM6YWNjb3VudC1hbGlhcyI6IlQzQksiLCJ1cm46dGllcjM6bG9jYXRpb24tYWxpYXMiOiJWQTEifQ.K9_YevPF_2iFnOI-2_nht8YRm_4CY1OFk8eMlXI1IgA">>.

req_token(Req) ->
	case cowboy_req:header(<<"authorization">>, Req) of
		{<<"Bearer ", Token/binary>>, _Req} -> Token;
    _ -> <<>>
	end.
