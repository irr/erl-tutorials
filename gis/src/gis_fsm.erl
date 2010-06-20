-module(gis_fsm).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_fsm).

-export([start_link/0, start_link/1, init/1, stop/0, stop/1,
         initial/2, initial/3, middle/2, middle/3,
         handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-record(state, {s = initial}).

start_link() ->
    start_link(gis_fsm).

start_link(Id) ->
    gen_fsm:start_link({local, Id}, ?MODULE, [], []).

init([]) ->
    {ok, initial, #state{}}.

initial({middle}, State) ->
    {next_state, middle, State#state{s = middle}}.

initial({middle}, _From, State) ->
    {reply, ok, middle, State#state{s = middle}}.

middle({initial}, State) ->
    {next_state, initial, State#state{s = initial}}.

middle({initial}, _From, State) ->
    {reply, ok, initial, State#state{s = initial}}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

stop() ->
    stop(gis_fsm).

stop(Id) ->
    gen_fsm:send_all_state_event(Id, stop).
