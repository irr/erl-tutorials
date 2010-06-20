-module(sql_sup).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(supervisor).

-export([start_link/0, init/1, child_spec/1, child_spec/3, start_child/1, stop_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [sql]).

init([Id]) when is_atom(Id) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [child_spec(Id)]}}.


child_spec(Id) when is_atom(Id) ->
    child_spec(Id, sql, sql).


child_spec(Id, App, Mod) when is_atom(Id),
                              is_atom(App),
                              is_atom(Mod) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {Id, {App, start_link, [Id]},
     Restart, Shutdown, Type, [Mod]}.


start_child([Id]) when is_atom(Id) ->
    start_child(Id);

start_child(Id) when is_atom(Id) ->
    supervisor:start_child(?MODULE, gis_sup:child_spec(Id)).


stop_child([Id]) when is_atom(Id) ->
    stop_child(Id);

stop_child(Id) when is_atom(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).
