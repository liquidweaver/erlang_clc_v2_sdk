%% To use this
%%   You must have eunit
%%   You must not call any test something_test
%%   You may have one or both - setup/0, teardown/1
%%   Like eunit, You can use the result of setup/0 in teardown/1
%%
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_FIXTURE_NAME, a_test_).

?TEST_FIXTURE_NAME() ->
{foreach, fun maybe_call_setup/0, fun maybe_call_teardown/1,
[ { atom_to_list( F ), fun() -> X = maybe_call_before(), apply( ?MODULE, F, [] ), maybe_call_after_(X) end } || {F, _A} <- ?MODULE:module_info(exports),
            F =/= ?TEST_FIXTURE_NAME,
            F =/= test,
            F =/= module_info,
            F =/= setup, F =/= maybe_call_setup,
            F =/= teardown, F =/= maybe_call_teardown,
            F =/= maybe_call_function,
            F =/= before, F =/= maybe_call_before,
            F =/= after_, F =/= maybe_call_after_
    ]
}.

maybe_call_before() ->
  maybe_call_function(before, []).

maybe_call_after_(ResultOfBefore) ->
  maybe_call_function(after_, [ResultOfBefore]).

maybe_call_setup() ->
  maybe_call_function(setup, []).

maybe_call_teardown(ResultOfSetup) ->
  maybe_call_function(teardown, [ResultOfSetup]),
  meck:unload().

maybe_call_function(F, A) ->
  case erlang:function_exported(?MODULE, F, length(A)) of
    true -> apply(?MODULE, F, A);
    false -> nothing
  end.

-define( meck(Modules, Args), meck:new( Modules, Args ) ).
-define( meck(Modules), meck:new( Modules ) ).
-define( any, '_' ).
-define( wait(M, F, A, Timeout), meck:wait(M, F, A, Timeout) ).
-define( history(Module), ?debugFmt( "History for module ~p:~p~n", [Module, meck:history(Module)] ) ).

-define( stub(M,F,A, ReturnValue), meck:expect( M, F, A, ReturnValue ) ).
-define( seq(S), meck:seq(S) ).

-define( called( M, F, A), ?assert( meck:called( M, F, A ) ) ).
-define( capture_when(When, Method, Function, Arity, CapturePosition), meck:capture(case When of second -> 2; _ -> When end, Method, Function, ['_' || _ <- lists:seq(1, Arity)], CapturePosition) ).
-define( capture(Method, Function, Arity, CapturePosition), ?capture_when(first, Method, Function, Arity, CapturePosition) ).