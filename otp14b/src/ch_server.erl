-module(ch_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/1, free/1, show/0, redis/0]).
-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [10], []).

init(Args) ->
    [N | _] = Args,
    L = lists:map(fun(X) -> 
                      {ok, R} = eredis:start_link(),
                      {X, R}
                  end, lists:seq(0, N - 1)),
    {ok, dict:from_list([{channels, []}, {redis, dict:from_list(L)}, {n, N}, {i, -1}])}.

alloc(Channel) ->
    gen_server:call(?MODULE, {alloc, Channel}).

show() ->
    gen_server:cast(?MODULE, {show}).

free(Ch) ->
    gen_server:cast(?MODULE, {free, Ch}).

redis() ->
    gen_server:call(?MODULE, {redis}, ?TIMEOUT).

get_redis_state(State) ->
    {ok, M} = dict:find(redis, State),
    {ok, N} = dict:find(n, State),
    {ok, I} = dict:find(i, State),
    P = (I + 1) rem N,
    {ok, C} = dict:find(P, M),
    {P, C}.

handle_call({alloc, Ch}, _From, State) ->
    {ok, Chs} = dict:find(channels, State),
    Chs_new = [Ch | Chs],
    {reply, ok, dict:update(channels, fun(_) -> Chs_new end, State)};
handle_call({redis}, From, State) ->
    {P, C} = get_redis_state(State),
    R = eredis:q(C, ["PING"]),
    io:format("hash ~p => ~p [~p]~n", [From, P, R]), 
    {reply, ok, dict:update(i, fun(_) -> P end, State)}.

handle_cast({show}, State) ->
    io:format("state: ~p~n", [State]),
    {noreply, State};
handle_cast({free, Ch}, State) ->
    {ok, Chs} = dict:find(channels, State),
    Chs2 = lists:filter(fun(X) ->
                           if X == Ch -> false;
                              true -> true
                           end
                       end, Chs),
    {noreply, dict:update(channels, fun(_) -> Chs2 end, State)}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

