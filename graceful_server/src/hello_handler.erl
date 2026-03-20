-module(hello_handler).
-encoding(utf8).                    % força o compilador a ler o arquivo como UTF-8
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    timer:sleep(2000),

    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
        <<"Olá do Erlang OTP! Graceful shutdown perfeito ✅\n"/utf8>>,   % ← /utf8 força os bytes corretos
        Req0
    ),

    {ok, Req, State}.
