-module(ch_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/1, free/1, show/0]).
-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #{channels => []}}.

alloc(Channel) ->
    gen_server:call(?MODULE, {alloc, Channel}, ?TIMEOUT).

show() ->
    gen_server:cast(?MODULE, {show}).

free(Ch) ->
    gen_server:cast(?MODULE, {free, Ch}).

handle_call({alloc, Ch}, _From, #{channels := Chs} = State) ->
    Chs_new = [Ch | Chs],
    {reply, ok, State#{channels => Chs_new}}.

handle_cast({show}, State) ->
    io:format("state: ~p~n", [State]),
    {noreply, State};
handle_cast({free, Ch}, #{channels := Chs} = State) ->
    Chs2 = lists:filter(fun(X) ->
                           if X == Ch -> false;
                              true -> true
                           end
                       end, Chs),
    {noreply, State#{channels => Chs2}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

