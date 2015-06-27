-module(clc_v2_http_client).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #{}}.

handle_call(_Request, _From, State = #{gun_pid := GunPid}) ->
  NewGunPid = case is_process_alive(GunPid) of
    false ->
      { ok, Pid } = gun:open(host, port),
      Pid;
    true -> GunPid
  end,
  NewState = do_auth(State#{ gun_pid => NewGunPid }),
  {reply, ok, NewState};
handle_call(_Request, _From, State) ->
  { ok, GunPid } = gun:open(host, port),
  {reply, ok, State#{ gun_pid => GunPid}}.

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

do_auth(State = #{ gun_pid := GunPid }) ->
  AuthRef = gun:post(GunPid, <<"v2/authentication/login">>, [], jiffy:encode( #{ username => <<"username">>, password => <<"password">> })),
  AuthRespBody = gun:await_body(GunPid, AuthRef),
  maps:fold(fun(K, V, Acc) -> maps:put(binary_to_atom(K, utf8), V, Acc) end, State, jiffy:decode(AuthRespBody, [return_maps])).
