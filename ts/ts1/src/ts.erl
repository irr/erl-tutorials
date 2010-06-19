-module(ts).
-vsn(1.0).
-author('ivan.ribeiro@gmail.com').

-behaviour(gen_server).

-include("ts.hrl").

-export([start_link/0, stop/0, loop/3, init/1,
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

    {ok, Listen} = gen_tcp:listen(Port, [binary,
                                         {packet, 0},
                                         {nodelay, true},
                                         {reuseaddr, true},
                                         {active, false}]),

    {ok, Att} = apply(Module, ts_init, [#ts_mod{app = ?APP}]),

    State = #ts_state{tout = Timeout, mod = Module, att = Att},

    spawn_link(fun() -> start_acceptor(Listen, State) end),

    io:format("TS server started ~p...~n", [State]),

    {ok, State}.


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


start_acceptor(Listen, State) when is_record(State, ts_state) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Pid = spawn(fun() -> loop(Socket, State, #ts_process{}) end),
            ok = gen_tcp:controlling_process(Socket, Pid),
            Pid ! {ready, Socket},
            start_acceptor(Listen, State);
        {error, closed} ->
            ok
    end.


recv_buffer(<<>>, Bin) when is_binary(Bin) ->
    Bin;
recv_buffer(LastBin, Bin) when is_binary(LastBin),
                               is_binary(Bin) ->
    <<LastBin/binary, Bin/binary>>.


recv_ready(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).


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
            ok;
        {ready, Socket} ->
            recv_ready(Socket),
            loop(Socket, State, Process)
    after Timeout ->
            io:format("[~p] Server socket closed (timeout)~n", [self()]),
            gen_tcp:close(Socket)
    end.
