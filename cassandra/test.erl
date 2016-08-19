-module(test).
-author("ivan.ribeiro@gmail.com").
-compile(export_all).

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

run() ->
    {ok, Client} = cqerl:get_client("127.0.0.1:9042", [{keyspace, 'irr'}]),
    {ok, Result} = cqerl:run_query(Client, <<"SELECT * FROM rt_series;">>),
    cqerl:close_client(Client),
    print(Result).
   
print_next(empty_dataset) ->
    ok;
print_next({_, R}) ->
    io:format("~p~n", [cqerl:head(R)]),
    print_next(cqerl:next(R)).

print(Result) ->
    print_next(cqerl:next(Result)).

