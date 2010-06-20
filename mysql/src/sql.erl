-module(sql).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-export([start_link/0, start_link/1, exec/1, exec/2,
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mysql.hrl").

exec(Sql) ->
    exec(sql, Sql).

exec(Id, Sql) ->
    gen_server:call(Id, {exec, Sql}).

start_link() ->
    start_link(gis).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id], []).

init([Id]) ->
    {ok, Host} = application:get_env(sql, host),
    {ok, Port} = application:get_env(sql, port),
    {ok, Size} = application:get_env(sql, size),
    {ok, User} = application:get_env(sql, user),
    {ok, Password} = application:get_env(sql, password),
    {ok, Database} = application:get_env(sql, database),
    {ok, Encoding} = application:get_env(sql, encoding),

    {ok, _} = mysql:start_link(mysql,
                               Host,
                               Port,
                               User,
                               Password,
                               Database,
                               fun mysql_log/4,
                               Encoding),

    lists:foreach(fun(_) ->
                          {ok, _} = mysql:connect(mysql, Host, Port, User, Password,
                                                  Database, Encoding, true)
                  end, lists:seq(1, Size)),

    State = {{gen, self()}, {mysql, application:get_all_env()}},
    io:format("SQL started ~p (~p)...~n", [Id, State]),
    {ok, State}.

handle_call({exec, Sql}, _From, _State) ->
    Result = case mysql:transaction(mysql, fun() -> mysql:fetch(mysql, Sql) end) of
                 {atomic, {data, #mysql_result{rows=[]}}} ->
                     {ok, []};
                 {atomic, {data, #mysql_result{rows=Rows}}} ->
                     {ok, Rows};
                 {atomic, {updated, #mysql_result{affectedrows=N}}} ->
                     {ok, N};
                 {aborted, {Reason, {rollback_result, _Result}}} ->
                     {error, Reason}
             end,
    {reply, Result, _State};

handle_call(_Request, _From, State) ->
    {reply, {error, invalid_call}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

mysql_log(_, Level, Msg, Params) when is_atom(Level),
                                      Level =:= error orelse Level =:= warn ->
    io:format("(~p): ~p [~p]~n", [Level, Msg, Params]);
mysql_log(_, _, _, _) ->
    ignore.
