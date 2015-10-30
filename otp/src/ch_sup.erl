-module(ch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_all, intensity => 5, period => 3},
    ChildSpecs = [#{id => ch_server,
                    start => {ch_server, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [ch_server]}],
    {ok, {SupFlags, ChildSpecs}}.

