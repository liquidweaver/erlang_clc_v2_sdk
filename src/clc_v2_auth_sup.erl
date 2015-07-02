-module(clc_v2_auth_sup).

-behaviour(supervisor).

%% API functions
-export([
  start_link/0,
  create_worker/2
  ]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec create_worker( Username::binary(), Password::binary() ) -> { ok, clc_v2_auth:auth_ref() }.
create_worker(Username, Password) ->
  supervisor:start_child(?MODULE, [Username, Password]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 10},
          [#{id => clc_v2_auth,
             start => {clc_v2_auth, start_link, []}
            }]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
