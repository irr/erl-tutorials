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


%% NewData = [{response,{StatusCode,Body}}] 
%%        | [{response,{response,Head,Body}}] 
%%        | [{response,{already_sent,Statuscode,Size}}] 
%% Head = [HeaderOption] 
%% HeaderOption = {Option, Value} | {code, StatusCode} 

do(ModData) ->
    Data = ModData#mod.entity_body,
	Content = io_lib:fwrite("<html><body><pre>~p</pre></body>~n", [{ModData, Data}]),
    {proceed, [{response, {200, Content}}]}.
