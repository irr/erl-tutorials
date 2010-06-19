-module(echo).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-export([ts_init/1, ts_run/4]).

-include("ts.hrl").

ts_init(_App) ->
    {ok, {}}.

ts_run(Socket, Bin, _State, _Process) when is_binary(Bin),
                                           is_record(_State, ts_state),
                                           is_record(_Process, ts_process) ->
    ok = gen_tcp:send(Socket, Bin),
    {ok, <<>>}.
