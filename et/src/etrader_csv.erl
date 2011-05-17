-module(etrader_csv).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-export([read/3]).

-include("etrader.hrl").

-spec list_to_number (string()) -> float() | integer().
list_to_number(L) when is_list(L) ->
    try
        list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.

-spec read (string(), string(), integer()) -> {ok, array()}.
read(F, S, G) when is_list(F),
                   is_list(S),
                   is_integer(G) ->
    {ok, I} = file:open(F, [read_ahead]),
    readlines(I, S, array:new(), 0, G).


-spec readlines (pid(), string(), array(), integer(), integer()) -> {ok, array()}.
readlines(I, _, A, G, G) when is_integer(G),
                              G > 0 ->
    file:close(I),
    {ok, A};
readlines(I, S, A, P, G) when is_pid(I),
                              is_list(S),
                              is_tuple(A),
                              is_integer(P),
                              is_integer(G) ->
    case file:read_line(I) of
        {ok, F} ->
            case string:substr(F, 1, string:len(S)) of
                S ->
                    [N, D, O, H, L, C, V, _] = string:tokens(string:strip(F, right, $\n), ","),
                    R = #ohlc{name = N, date = D,
                              open = list_to_number(O),
                              high = list_to_number(H),
                              low = list_to_number(L),
                              close = list_to_number(C),
                              volume = list_to_number(V)},
                    readlines(I, S, array:set(P, R, A), P + 1, G);
                _ ->
                    readlines(I, S, A, P, G)
            end;
        eof ->
            file:close(I),
            {ok, A}
    end.


