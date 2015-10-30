-module(ch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {ch_server, {ch_server, start_link, []},
              Restart, Shutdown, Type, [ch_server]},

    {ok, {SupFlags, [AChild]}}.

