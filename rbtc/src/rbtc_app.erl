-module(rbtc_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(cowboy),
    application:start(rbtc).

start(_StartType, _StartArgs) ->
    Dispatch = [
                {'_', [
                       {'_', rbtc_handler, []}
                      ]}
               ],
    cowboy:start_listener(rbtc_listener, 100,
                          cowboy_tcp_transport, [{port, 8080}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    rbtc_sup:start_link().

stop(_State) ->
    ok.
