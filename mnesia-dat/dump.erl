-module(dump).
-author("ivan.ribeiro@gmail.com").
-compile(export_all).

create_test() ->
    Nodes=[node()],
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(test, [{frag_properties,
        [{node_pool, Nodes},
         {n_fragments, 20},
         {n_disc_only_copies, 1}]},
         {disc_only_copies, Nodes},
         {attributes, [key, val]}]),
    lists:foreach(fun(X) ->
        mnesia:activity(transaction,
            fun({Key, Val}) ->
                mnesia:write({test, Key, Val})
            end, [{X, X * 1000}],
        mnesia_frag) end, lists:seq(1, 1000)),
    mnesia:activity(transaction,
        fun() ->
            mnesia:table_info(test, size)
        end, [], mnesia_frag).

main([File]) when is_list(File) ->
    process(File);

main(_) ->
    usage().

usage() ->
    io:format("usage: escript dump.erl <file>\n"),
    halt(1).

process(File) ->
   {ok, N} = dets:open_file(schema, [{file, File},{repair,false}, {keypos, 2}]),
   F = fun(X) -> io:format("~p~n", [X]), continue end,
   dets:traverse(N, F),
   dets:close(N).
