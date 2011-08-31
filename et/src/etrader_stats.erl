-module(etrader_stats).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-export([sma/2, ema/2]).

-export_type([ma_t/0]).
-type ma_t() :: {queue(), array()}.

-include("etrader.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% R:
%% library(quantmod)
%% uol <- read.csv(file="/data/bovespa/etrader.csv", header=FALSE)
%% EMA(uol[[5]][1:100], n=21)

-spec mm (integer(), integer(), integer(), ma_t()) -> ma_t().
mm(I, N, N, {Q, MA}) when is_integer(I),
                          is_integer(N),
                          is_tuple(Q),
                          is_tuple(MA) ->
    Sum = lists:foldl(fun(X, A) -> X + A end, 0, queue:to_list(Q)),
    {_, NewQ} = queue:out(Q),
    {NewQ, array:set(I, Sum / N, MA)};
mm(_, _, _, T) when is_tuple(T) ->
    T.

-spec em (integer(), integer(), integer(), ma_t()) -> ma_t().
em(I, N, N, {Q, MA}) when is_integer(I),
                          is_integer(N),
                          is_tuple(Q),
                          is_tuple(MA) ->
    case array:get(N - 1, MA) of
        undefined ->
            mm(I, N, N, {Q, MA});
        _ ->
            X = 2 / (N + 1),
            PM = array:get(I - 1, MA),
            {{value, V}, NewQ} = queue:out_r(Q),
            EMA = V * X + PM * (1 - X),
            {NewQ, array:set(I, EMA, MA)}
    end;
em(_, _, _, T) when is_tuple(T) ->
    T.

-spec ma (integer(), array(), integer(), integer(), ma_t(), function()) -> array().
ma(0, _, _, _, _, _) ->
    array:new();
ma(L, _, _, I, {_, MA}, _) when I =:= L,
                                is_integer(I),
                                is_integer(L),
                                is_tuple(MA) ->
    MA;
ma(L, A, N, I, {Q, MA}, F) when is_integer(L),
                                is_tuple(A),
                                is_integer(N),
                                is_integer(I),
                                is_tuple(Q),
                                is_tuple(MA),
                                is_function(F, 4)->
    T = array:get(I, A),
    Val = T#ohlc.close,
    NewQ = queue:in(Val, Q),
    ma(L, A, N, I + 1, F(I, N, queue:len(NewQ), {NewQ, MA}), F).

-spec sma (array(), integer()) -> ma_t().
sma(A, N) ->
    ma(array:size(A), A, N, 0, {queue:new(), array:new(array:size(A))}, fun mm/4).

-spec ema (array(), integer()) -> ma_t().
ema(A, N) ->
    ma(array:size(A), A, N, 0, {queue:new(), array:new(array:size(A))}, fun em/4).

-ifdef(TEST).
%% erl -make && erl -pa ../ebin +K true +A 42 +B -run etrader_app start -etrader limit 100
%% ps.: limit MUST BE 100 to pass tests!
ma_test() ->
    {ok, [DATA]} = file:consult(?TEST_DATA),
    EMA = ema(DATA, 21),
    {ok, [EMA_TEST]} = file:consult(?TEST_EMA21),
    ?assertEqual(EMA, EMA_TEST),
    SMA = sma(DATA, 21),
    {ok, [SMA_TEST]} = file:consult(?TEST_SMA21),
    ?assertEqual(SMA, SMA_TEST).
-endif.


