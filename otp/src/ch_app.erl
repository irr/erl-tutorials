-module(ch_app).
-behaviour(application).
-export([from_shell/0, start/2, stop/1]).

from_shell() ->
    % erl -pa ebin/ -run ch_app from_shell
    application:ensure_all_started(?MODULE).

start(_Type, _StartArgs) ->
    case ch_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
