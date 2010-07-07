-module(test).

-export([start/0, stop/0, init/1]).

-export([f/1]).

-define(REG, ?MODULE).

start() ->
    start("test_drv").

start(SharedLib) ->
    init(erl_ddll:load_driver("../priv", SharedLib), SharedLib).

init(ok, SharedLib) ->
    init(SharedLib);
init({error, already_loaded}, _) ->
    {ok, ?REG};
init(_, _) ->
    exit({error, could_not_load_driver}).

init(SharedLib) ->
    register(?REG,
             spawn_link(fun() ->
                                loop(open_port({spawn, SharedLib}, []))
                        end)),
    {ok, ?REG}.

stop() ->
    ?REG ! stop.

f(X) ->
    R = call_port({f, X}),
    io:format("f(~p) = ~p~n", [X, R]),
    R.

call_port(Msg) ->
    ?REG ! {call, self(), Msg},
    receive
        {?REG, Result} ->
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {?REG, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
            exit(port_terminated)
    end.

encode({f, X}) ->
    [1, X].

decode([Int]) ->
    Int.
