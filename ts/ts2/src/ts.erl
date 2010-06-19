%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% hacking on thrift_server.erl code
%% (http://svn.apache.org/viewvc/incubator/thrift/trunk/)

-module(ts).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-include("ts.hrl").

-export([start_link/0, stop/0, start_loop/1, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


stop() ->
    gen_server:cast(?SERVER, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, Module} = application:get_env(?APP, module),
    {ok, Port} = application:get_env(?APP, port),
    {ok, Timeout} = application:get_env(?APP, timeout),

    {ok, Socket} = gen_tcp:listen(Port, [binary,
                                         {packet, 0},
                                         {nodelay, true},
                                         {reuseaddr, true},
                                         {active, false}]),

    {ok, Ref} = prim_inet:async_accept(Socket, -1),

    {ok, Att} = apply(Module, ts_init, [#ts_mod{app = ?APP}]),

    State = #ts_state{lsock = Socket, aref = Ref, tout = Timeout, mod = Module, att = Att},

    io:format("TS server started ~p...~n", [State]),

    {ok, State}.


handle_call({controlling_process, Socket}, {FromPid, _}, State) ->
    {reply, gen_tcp:controlling_process(Socket, FromPid), State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, ListenSocket, _Ref, {ok, ClientSocket}}, State) ->
    case set_sockopt(ListenSocket, ClientSocket) of
        ok ->
            start_processor(ClientSocket, State),
            {ok, NewRef} = prim_inet:async_accept(ListenSocket, -1),
            {noreply, State#ts_state{aref = NewRef}};
        {error, Reason} ->
            error_logger:error_msg("Couldn't set socket opts: ~p~n",
                                   [Reason]),
            {stop, Reason, State}
    end;

handle_info({inet_async, _, _, Error}, State) ->
    error_logger:error_msg("Error in acceptor: ~p~n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


set_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket,
                           [active,
                            nodelay,
                            keepalive,
                            delay_send,
                            priority,
                            tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(ClientSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(ClientSocket),
                         Error
            end;
        Error ->
            gen_tcp:close(ClientSocket),
            Error
    end.


start_processor(Socket, State) when is_record(State, ts_state) ->
    Server = self(),
    Init = fun() ->
                   ok = gen_server:call(Server, {controlling_process, Socket}),
                   {ok, Socket, State}
           end,
    spawn(?MODULE, start_loop, [Init]).


start_loop(Init) when is_function(Init, 0) ->
    {ok, Socket, State} = Init(),
    recv_ready(Socket),
    loop(Socket, State, #ts_process{}).


recv_ready(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).


recv_buffer(<<>>, Bin) when is_binary(Bin) ->
    Bin;
recv_buffer(LastBin, Bin) when is_binary(LastBin),
                               is_binary(Bin) ->
    <<LastBin/binary, Bin/binary>>.


loop(Socket, State, Process) when is_record(State, ts_state),
                                  is_record(Process, ts_process) ->
    Timeout = State#ts_state.tout,
    receive
        {tcp, Socket, Bin} when is_binary(Bin) ->
            NewBin = recv_buffer(Process#ts_process.lbin, Bin),
            {ok, LBin} = apply(State#ts_state.mod, ts_run, [Socket, NewBin, State, Process]),
            recv_ready(Socket),
            loop(Socket, State, Process#ts_process{lbin = LBin});
        {tcp_closed, Socket} ->
            ok
    after Timeout ->
            io:format("[~p] Server socket closed (timeout)~n", [self()]),
            gen_tcp:close(Socket)
    end.
