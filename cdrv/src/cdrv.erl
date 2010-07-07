-module(cdrv).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-export([start_link/0, start_link/1, f/1, f/2, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

f(X) ->
	f(cdrv, X).

f(Id, X) ->
	gen_server:call(Id, {f, X}).

start_link() ->
	start_link(cdrv).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id], []).

init([Id]) ->
	Driver = test:start(),
	State = {gen, self(), port, Driver},
    io:format("CDRV started ~p (~p)...~n", [Id, State]),
    {ok, State}.

handle_call({f, X}, _From, State) when is_integer(X) ->
	Res = test:f(X),
	io:format("f called: ~p~n", [{State, Res}]),
    {reply, {ok, X}, {State, X}};

handle_call(_Request, _From, State) ->
    {reply, {error, invalid_number}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
