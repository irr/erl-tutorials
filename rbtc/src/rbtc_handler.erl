-module(rbtc_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    spawn(fun() -> redo:start_link() end),
    {ok, Req, undefined_state}.

handle(Req, State) ->
    case redo:cmd(["PING"]) of
        <<"PONG">> = R ->
            {ok, Req2} = cowboy_http_req:reply(200, [], R, Req);
        E ->
            {ok, Req2} = cowboy_http_req:reply(500, [], E, Req)
    end,
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
