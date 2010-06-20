-module(gis).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/1, test/2, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

stop(Id) ->
    gen_server:cast(Id, stop).

test(Id, X) ->
	gen_server:call(Id, {test, X}).


start_link() ->
	start_link(gis).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).

init([]) ->
	State = {self()},
    io:format("GIS started ~p...~n", [State]),
    {ok, State}.


handle_call({test, X}, _From, State) ->
	io:format("test called: ~p~n", [{State}]),
    {reply, {ok, X}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
	io:format("stop called: ~p~n", [{State}]),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
