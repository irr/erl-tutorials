-module(srt).
-author("irocha@uolinc.com").
-compile(export_all).

main([File]) when is_list(File) ->
    C = os:cmd("file -bi " ++ File),
    [_, T] = string:tokens(C, "="),
    process(File, string:strip(string:strip(T), both, $\n));

main(_) ->
    usage().

usage() ->
    io:format("usage: escript srt.erl <file>\n"),
    halt(1).

process(File, Encoding) ->
    Charset = string:to_upper(Encoding),
    List = string:tokens(os:cmd("iconv -l"), "//\n"),
    process(File, Charset, [C || C <- List, C == Charset]).

process(_File, Charset, []) ->
    throw(io:format("Invalid encoding [~p]\n", [Charset]));

process(File, Charset, _) ->    
    Content = os:cmd("iconv -f " ++ Charset ++ " -t ISO885915//TRANSLIT " ++ File),
    Srt = re:replace(Content, "<.*?i>|<.*?b>|<.*?u>", "", [global, caseless, {return,list}]),
    ok = file:write_file(File, Srt).
    