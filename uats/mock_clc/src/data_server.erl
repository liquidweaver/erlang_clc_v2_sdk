-include("data.hrl").
-module(data_server).

-behaviour(gen_server).

-export([get/1,
         get/2,
         put/2,
         put/3,
         clear/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

init([]) ->
  {ok, empty_state()}.

get(Type) ->
  gen_server:call(?DATASERVER, Type).

get(Type, Id) ->
  gen_server:call(?DATASERVER, {Type, Id}).

put(Type, Value) ->
  gen_server:cast(?DATASERVER, {put, Type, Value}).

put(Type, Id, Value) ->
  gen_server:cast(?DATASERVER, {put, Type, Id, Value}).

clear() ->
  gen_server:call(?DATASERVER, clear).

handle_call({Type, Id}, _From, State) ->
  Types = maps:get(Type, State),
  {reply, maps:get(Id, Types), State};
handle_call(Type, _From, State) ->
  {reply, maps:get(Type, State), State}.

handle_cast({put, Type, Value}, State) ->
  {noreply, maps:put(Type, Value, State)};
handle_cast({put, Type, Id, Value}, State) ->
  {noreply, maps:put(Type, #{ Id => Value }, State)};
handle_cast(clear, _State) ->
  {noreply, empty_state()}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

empty_state() ->
  #{ }.
