-module(ws).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-include_lib("inets/src/httpd.hrl").

-include("ws.hrl").

-export([start_link/0, stop/0, init/1, do/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONTENT_TYPE, "plain/text; charset=ISO-8859-1").


stop() ->
    gen_server:cast(?SERVER, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    {ok, Modules} = application:get_env(?APP, modules),
    {ok, Port} = application:get_env(?APP, port),
    {ok, Bind} = application:get_env(?APP, bind),
    {ok, Name} = application:get_env(?APP, name),
    {ok, SRoot} = application:get_env(?APP, server_root),
    {ok, DRoot} = application:get_env(?APP, document_root),

    Config = [{port, Port},
              {server_root, SRoot},
              {document_root, DRoot},
              {bind_address, Bind},
              {server_name, Name},
              {modules, Modules}],

    {ok, _} = inets:start(httpd, Config, stand_alone),

    io:format("WS started ~p...~n", [Config]),

    {ok, Config}.


handle_call(_Request, _From, State) ->
    {noreply, State}.

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


do(ModData) ->
    Data = ModData#mod.entity_body,
    Body = lists:flatten(io_lib:fwrite("WS (data received):~n~p~n",
                                       [{ModData, Data}])),
    Head = [{content_length, integer_to_list(length(Body))},
            {content_type, ?CONTENT_TYPE},
            {code, 200}],
    {proceed, [{response, {response, Head, Body}}]}.
