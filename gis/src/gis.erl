-module(gis).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-export([start_link/0, start_link/1, test/2, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

test(Id, X) ->
	gen_server:call(Id, {test, X}).

start_link() ->
	start_link(gis).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id], []).

init([Id]) ->
	Name = list_to_atom(atom_to_list(Id) ++ "_fsm"),
	{ok, Pid} = gis_fsm:start_link(Name),
	State = {gen, self(), fsm, Pid},
    io:format("GIS started ~p (~p)...~n", [Name, State]),
    {ok, State}.

handle_call({test, X}, _From, State) when is_integer(X) ->
	io:format("test called: ~p~n", [{State}]),
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
