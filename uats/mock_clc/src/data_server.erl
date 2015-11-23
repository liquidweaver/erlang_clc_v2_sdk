-include("data.hrl").
-module(data_server).

-behaviour(gen_server).

-export([get/1,
         put/2,
         clear/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

init([]) ->
  {ok, empty_state()}.

get(Key) ->
  gen_server:call(?DATASERVER, Key).

put(Key, Value) ->
  gen_server:cast(?DATASERVER, {put, Key, Value}).

clear() ->
  gen_server:call(?DATASERVER, clear).

handle_call(Key, _From, State) ->
  {reply, maps:get(Key, State), State}.

handle_cast({put, Key, Value}, State) ->
  {noreply, maps:put(Key, Value, State)};
handle_cast(clear, _State) ->
  {noreply, empty_state()};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

empty_state() ->
  #{ alert_policies => #{ items => [] }}.
