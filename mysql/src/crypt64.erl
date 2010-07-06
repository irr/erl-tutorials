-module(crypt64).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-export([decode/1, encode/1]).

-record(crypt64, {k = <<"alessandrasantos">>,
                  v = <<"=irr@github.com=">>,
                  p = 64,
                  s = 32}).

-define(CRYPT64_CTRL, #crypt64{}).

-define(CRYPT64_ERROR(M, X), lists:flatten(io_lib:format(M, [X]))).

-define(CRYPT64_INVALID, "\n").
-define(CRYPT64_MAXSIZE, 32).

encode(0, P, N) when is_list(P),
                     is_integer(N),
                     N > 0,
                     N =< ?CRYPT64_MAXSIZE ->
    C = ?CRYPT64_CTRL,
    L = lists:concat([P,
                      ?CRYPT64_INVALID,
                      [(B rem 20) + $a || B <- binary_to_list(crypto:rand_bytes(C#crypt64.p - (N + 1)))]]
                    ),
    {ok, base64:encode_to_string(crypto:aes_cbc_128_encrypt(C#crypt64.k, C#crypt64.v, L))};
encode(N, _, _) when N > 0 ->
    {error, ?CRYPT64_ERROR("invalid password character ~p", ?CRYPT64_INVALID)};
encode(_, _, _) ->
    {error, ?CRYPT64_ERROR("password exceeds ~p characters", ?CRYPT64_MAXSIZE)}.


encode(P) when is_list(P) ->
    encode(string:str(P, ?CRYPT64_INVALID), P, length(P)).

decode(S) when is_list(S) ->
    C = ?CRYPT64_CTRL,
    L = binary_to_list(crypto:aes_cbc_128_decrypt(C#crypt64.k, C#crypt64.v, base64:decode(S))),
    [R | _] = string:tokens(L, ?CRYPT64_INVALID),
    {ok, R}.

