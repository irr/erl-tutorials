-module(cdrv_app).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
	application:start(cdrv).

start(_StartType, _StartArgs) ->
	case 'cdrv_sup':start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
				end.

stop(_State) ->
	ok.
