-module(ws_sup).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("ws.hrl").

-define(SERVER, ?MODULE).


start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent,
	Shutdown = 2000,
	Type = worker,

	AChild = {?APP, {?APP, start_link, []},
			  Restart, Shutdown, Type, [?APP]},

	{ok, {SupFlags, [AChild]}}.
