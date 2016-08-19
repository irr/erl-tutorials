-module(test).
-author("ivan.ribeiro@gmail.com").
-compile(export_all).

%% erl +B -pa ~/git/cqerl/ebin ~/git/cqerl/deps/*/ebin

setup() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(pooler),
    ok = application:start(re2),
    ok = application:start(semver),
    ok = application:start(snappy),
    ok = application:start(lz4),
    ok = application:start(quickrand),
    ok = application:start(uuid),
    ok = application:start(cqerl).

exec(Client) ->
    {ok, Result} = cqerl:run_query(Client, <<"SELECT * FROM rt_series;">>),
    print(Result).

run() ->
    {ok, Client} = cqerl:get_client("127.0.0.1:9042", [{keyspace, 'irr'}]),
    exec(Client).

run_cluster() ->
    cqerl_cluster:add_nodes([{ "127.0.0.1", 9042}], [{keyspace, 'irr'}]),
    {ok, Client} = cqerl_cluster:get_any_client(),
    exec(Client).
   
print_next(empty_dataset) ->
    ok;
print_next({_, R}) ->
    io:format("~p~n", [cqerl:head(R)]),
    print_next(cqerl:next(R)).

print(Result) ->
    print_next(cqerl:next(Result)).

