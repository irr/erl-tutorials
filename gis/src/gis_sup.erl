-module(gis_sup).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(supervisor).

-export([start_link/0, init/1, child_spec/1, child_spec/3, add_child/1, remove_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [gis]).

init([Id]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [child_spec(Id)]}}.


child_spec(Id) ->
	child_spec(Id, gis, gis).


child_spec(Id, App, Mod) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {Id, {App, start_link, [Id]},
     Restart, Shutdown, Type, [Mod]}.


add_child(Id) ->
    supervisor:start_child(?MODULE, gis_sup:child_spec(Id)).


remove_child(Id) ->
	supervisor:terminate_child(?MODULE, Id),
	supervisor:delete_child(?MODULE, Id).
