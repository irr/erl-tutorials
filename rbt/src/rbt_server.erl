-module(rbt_server).

-behaviour(gen_server).

-include_lib("inets/src/http_server/httpd.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, do/1, state/0]).

-define(SERVER, ?MODULE).
-define(CONTENT_TYPE, "application/json").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Config = [{port, 1972},
              {server_root, "/tmp"},
              {document_root, "/tmp"},
              {bind_address, {127,0,0,1}},
              {server_name, "rbt"},
              {modules, [rbt_server]}],
    {ok, _} = inets:start(httpd, Config, stand_alone),
    io:format("~nRBT started ~p...~n", [Config]),
    init_redis(),
    {ok, Config}.


init_redis() ->
    spawn(fun() -> redo:start_link() end).

state() ->
    gen_server:call(?MODULE, state).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

exec(_ModData) ->
    case redo:cmd(["PING"]) of
        <<"PONG">> ->
            ok;
        _ ->
            error
    end.

do(ModData) ->
    try exec(ModData) of
        error ->
            Head = [{content_length, "0"},
                    {content_type, ?CONTENT_TYPE},
                    {server, proplists:get_value(server_name, state())},
                    {code, 404}],
            init_redis(),
            {proceed, [{response, {response, Head, []}}]};
        _ ->
            Data = ModData#mod.entity_body,
            Pairs = [string:tokens(X, "=") || X <- string:tokens(Data, "&")],
            Struct = {struct, [{list_to_atom(hd(X)), list_to_binary(http_uri:decode(hd(tl(X))))} || X <- Pairs]},
            Json = iolist_to_binary(mochijson2:encode(Struct)),
            Body = lists:flatten(io_lib:fwrite("~p", [binary_to_list(Json)])),
            Head = [{content_length, integer_to_list(length(Body))},
                    {content_type, ?CONTENT_TYPE},
                    {server, proplists:get_value(server_name, state())},
                    {code, 200}],
            {proceed, [{response, {response, Head, Body}}]}
    catch
        _:_ ->
            Head = [{content_length, "0"},
                    {content_type, ?CONTENT_TYPE},
                    {server, proplists:get_value(server_name, state())},
                    {code, 500}],
            init_redis(),
            {proceed, [{response, {response, Head, []}}]}
    end.
