-module(ts_app).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(application).

-export([start/0, start/2, stop/1]).

-include("ts.hrl").


start() ->
	application:start(?APP).

start(_StartType, _StartArgs) ->
	case 'ts_sup':start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
				end.

stop(_State) ->
	ok.
