-module(clc_v2_auth).

-behaviour(gen_server).

%% API functions
-export([
  start_link/2,
  user_info/1
  ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-opaque auth_ref() :: pid().
-type user_info() :: map().

-export_type( [auth_ref/0, user_info/0] ).
%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Username, Password) ->
  gen_server:start_link(?MODULE, [Username, Password], []).

-spec user_info( ApiRef::auth_ref() ) -> user_info().
user_info(ApiRef) ->
  gen_server:call( ApiRef, user_info ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Username, Password]) ->
  {ok, { Username, Password, unset }}.

handle_call(user_info, _From, { Username, Password, unset }) ->
  { ok, UserInfo } = clc_v2_authentication:login( Username, Password ),
  {reply, UserInfo, { Username, Password, UserInfo }};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
