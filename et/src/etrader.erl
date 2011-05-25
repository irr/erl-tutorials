-module(etrader).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-include("etrader.hrl").

-export([start_link/0, stop/0, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3,
         source/1, load/1, sma/1, ema/1]).

-define(SERVER, ?MODULE).

stop() ->
    gen_server:cast(?SERVER, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, File} = application:get_env(?APP, csv),
    {ok, Timeout} = application:get_env(?APP, timeout),
    Limit = proplists:get_value(limit, application:get_all_env(?APP), 0),
    State = #etrs{csv = File, timeout = Timeout, limit = Limit},
    io:format("etrader server started ~p...~n", [State]),
    {ok, State}.

timeout() ->
    {ok, Timeout} = application:get_env(?APP, timeout),
    Timeout.

%% =======================
%%          API
%% =======================

-spec load (list()) -> {ok, integer()}.

source(File) when is_list(File) ->
    gen_server:call(?SERVER, {source, File}).

load(Symbol) when is_list(Symbol) ->
    gen_server:call(?SERVER, {load, Symbol}, timeout()).

-spec sma (integer()) -> {ok, array()}.
sma(N) ->
    gen_server:call(?SERVER, {ma, N, fun etrader_stats:sma/2}, timeout()).

-spec ema (integer()) -> {ok, array()}.
ema(N) ->
    gen_server:call(?SERVER, {ma, N, fun etrader_stats:ema/2}, timeout()).

%% =======================

state(S) ->
    Size = case S#etrs.data of
               undefined ->
                   0;
               A ->
                   array:size(A)
           end,
    io:format("State: {csv=~p, timeout=~p, limit=~p, data(size)=~p~n",
              [S#etrs.csv, S#etrs.timeout, S#etrs.limit, Size]),
    S.

handle_call({load, Symbol}, _From, State) ->
    {ok, A} = etrader_csv:read(State#etrs.csv, Symbol, State#etrs.limit),
    NewState = State#etrs{data = A},
    {reply, {ok, array:size(A)}, state(NewState)};

handle_call({ma, N, F}, _From, State) ->
    state(State),
    {reply, {ok, apply(F, [State#etrs.data, N])}, state(State)};

handle_call({source, File}, _From, State) ->
    application:set_env(?APP, csv, File),
    NewState = State#etrs{csv = File},
    {reply, ok, state(NewState)};

handle_call(_Request, _From, State) ->
    {noreply, State}.

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
