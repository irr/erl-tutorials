-module(graceful_server_app).
-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),

    ListenerName = my_http_listener,

    {ok, _} = cowboy:start_clear(ListenerName,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}),

    io:format("🚀 Servidor Erlang OTP Cowboy rodando em http://0.0.0.0:8080~n"),
    io:format("   Teste com: curl http://localhost:8080~n"),

    {ok, SupPid} = graceful_server_sup:start_link(),
    {ok, SupPid, #{listener => ListenerName}}.

%% === GRACEFUL SHUTDOWN AUTOMÁTICO DO OTP ===
prep_stop(State = #{listener := ListenerName}) ->
    io:format("~n🛑 OTP iniciou graceful shutdown...~n"),

    ranch:set_max_connections(ListenerName, 0),

    drain_connections(ListenerName, 30),

    cowboy:stop_listener(ListenerName),

    io:format("✅ Graceful shutdown completo! Todas as conexões foram drenadas.~n"),
    State.

stop(_State) ->
    ok.

%% Helper de drenagem
drain_connections(_, 0) ->
    io:format("   Timeout de 30s atingido. Forçando fechamento.~n");
drain_connections(ListenerName, SecsLeft) ->
    Active = ranch_server:count_connections(ListenerName),
    case Active of
        0 ->
            io:format("   Todas as conexões drenadas com sucesso!~n");
        N ->
            io:format("   ~p conexões ainda ativas... aguardando 1s (~p s restantes)~n", [N, SecsLeft]),
            timer:sleep(1000),
            drain_connections(ListenerName, SecsLeft - 1)
    end.
