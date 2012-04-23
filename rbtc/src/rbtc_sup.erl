-module(rbtc_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 10, 10}, []} }.

