-module(ch_app).
-behaviour(application).
-export([from_shell/0, start/2, stop/1]).

from_shell() ->
    % rebar get-deps
    % rebar compile
    % erl -pa ebin/ -pa deps/*/ebin -run ch_app from_shell
    application:ensure_all_started(?MODULE),
    io:format("~nch_server:show().~n", []),
    io:format("~nch_server:redis().~n", []),
    io:format("~nlists:foreach(fun(_) -> spawn(ch_server, redis, []) end, lists:seq(1, 5)).~n", []),
    ch_server:redis().

start(_Type, _StartArgs) ->
    case ch_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
