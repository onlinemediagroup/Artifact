%
% Artifact Data Store
%
% Copyright (c) 2012 Zephyr Pellerin
% All rights reserved.
%
% Redistribution and use in source and binary forms are permitted
% provided that the above copyright notice and this paragraph are
% duplicated in all such forms and that any documentation,
% advertising materials, and other materials related to such
% distribution and use acknowledge that the software was developed
% by Zephyr Pellerin.  My name cannot be used in endorsement of any product
% derived from this software. Buy me a beer sometime if you liked this
% THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
% IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. 
%

-module(artifact_stat).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([all/0, incr_cmd_get/0, incr_cmd_set/0, add_bytes_read/1,
         add_bytes_write/1, incr_unreconciled_get/1]).
-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3
        ]).

-include("artifact.hrl").
-record(state, {
          boot_time, cmd_get, cmd_set, bytes_read, bytes_write,
          unreconciled_get, node, quorum,
          number_of_buckets, number_of_virtual_nodes, store
}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).

init(_Args) ->
    {Msec, Sec, _Usec} = now(),
    BootTime = 1000000 * Msec + Sec,
    [LocalNode, N, R, W, NumberOfBuckets, NumberOfVirtualNodes, Store] =
        artifact_config:get([
            node, n, r, w, number_of_buckets, number_of_virtual_nodes, store
        ]),
    Quorum = join(",", [integer_to_list(X) || X <- [N, R, W]]),
    {ok, #state{
       boot_time               = BootTime,
       cmd_get                 = 0,
       cmd_set                 = 0,
       bytes_read              = 0,
       bytes_write             = 0,
       unreconciled_get        = array:new([{size, N-1}, {fixed, false},
                                            {default, [0,0]}]),
       node                    = LocalNode,
       quorum                  = Quorum,
       number_of_buckets       = NumberOfBuckets,
       number_of_virtual_nodes = NumberOfVirtualNodes,
       store                   = Store
      }}.

terminate(_Reason, _State) ->
    ok.

join(_Delimiter, [], Acc) ->
    Acc;
join(_Delimiter, [Token], Acc) ->
    Acc ++ Token;
join(Delimiter, [Token|Rest], Acc) ->
    join(Delimiter, Rest, Acc ++ Token ++ Delimiter).
join(Delimiter, List) ->
    join(Delimiter, List, []).

node_to_list({{A1,A2,A3,A4}, Port}) ->
    Addr = join(".", [integer_to_list(X) || X <- [A1,A2,A3,A4]]),
    Addr ++ ":" ++ integer_to_list(Port).

format_unreconciled_get(UnreconciledGet) ->
    join(" ",
         array:foldr(
           fun(_Index, [UnReconciled, InternalNum], Acc) ->
                   [io_lib:format("~B(~B)", [UnReconciled, InternalNum]) | Acc]
           end, [], UnreconciledGet)).

stat(Name, State) ->
    case Name of
        uptime ->
            {Msec, Sec, _Usec} = now(),
            Uptime = 1000000 * Msec + Sec - State#state.boot_time,
            {uptime, integer_to_list(Uptime)};
        time ->
            {Msec, Sec, _Usec} = now(),
            Time = 1000000 * Msec + Sec,
            {time, integer_to_list(Time)};
        version ->
            Version =
                case application:get_key(artifact, vsn) of
                    {ok, V} -> V;
                    _       -> "0"
                end,
            {version, Version};
        bytes ->
            Bytes = artifact_store:info(bytes),
            {bytes, integer_to_list(Bytes)};
        curr_items ->
            Size = artifact_store:info(size),
            {curr_items, integer_to_list(Size)};
        curr_connections ->
            Connections = artifact_tcp_server:info(artifact_memcache, curr_connections),
            {curr_connections, integer_to_list(Connections)};
        cmd_get ->
            {cmd_get, integer_to_list(State#state.cmd_get)};
        cmd_set ->
            {cmd_set, integer_to_list(State#state.cmd_set)};
        bytes_read ->
            {bytes_read, integer_to_list(State#state.bytes_read)};
        bytes_write ->
            {bytes_write, integer_to_list(State#state.bytes_write)};
        artifact_node ->
            {artifact_node, node_to_list(State#state.node)};
        artifact_quorum ->
            {artifact_quorum, State#state.quorum};
        artifact_number_of_buckets ->
            NumberOfBuckets = State#state.number_of_buckets,
            {artifact_number_of_buckets, integer_to_list(NumberOfBuckets)};
        artifact_number_of_virtual_nodes ->
            NumberOfVirtualNodes = State#state.number_of_virtual_nodes,
            {artifact_number_of_virtual_nodes,
             integer_to_list(NumberOfVirtualNodes)};
        artifact_store ->
            {artifact_store, atom_to_list(State#state.store)};
        artifact_curr_nodes ->
            {node_list, Nodes} = artifact_hash:node_list(),
            NodesInList = lists:sort([node_to_list(N) || N <- Nodes]),
            {artifact_curr_nodes, join(" ", NodesInList)};
        artifact_unreconciled_get ->
            {artifact_unreconciled_get,
             format_unreconciled_get(State#state.unreconciled_get)};
%        artifact_curr_buckets ->
%            {buckets, Buckets} = artifact_hash:buckets(),
%            {artifact_curr_buckets, join(" ", [integer_to_list(B) || B <- Buckets])};
        erlang_procs ->
            ErlangProcs = erlang:system_info(process_count),
            {erlang_procs, integer_to_list(ErlangProcs)};
        erlang_version ->
            {erlang_version, erlang:system_info(version)}
    end.

all(State) ->
    Stats =
        lists:map(
          fun(Name) -> stat(Name, State) end,
          [uptime, time, version, bytes,
           curr_items, curr_connections, cmd_get, cmd_set,
           bytes_read, bytes_write,
           artifact_node, artifact_quorum, artifact_number_of_buckets,
           artifact_number_of_virtual_nodes, artifact_store, artifact_curr_nodes,
           artifact_unreconciled_get,
           %artifact_curr_buckets,
           erlang_procs, erlang_version]
         ),
    {reply, Stats, State}.

incr_cmd_get(State) ->
    State2 = State#state{cmd_get = State#state.cmd_get + 1},
    {noreply, State2}.

incr_cmd_set(State) ->
    State2 = State#state{cmd_set = State#state.cmd_set + 1},
    {noreply, State2}.

incr_unreconciled_get(InternalNum, Reconciled, State) ->
    Old = State#state.unreconciled_get,
    Index = InternalNum -2,  % index is 0: two versions, 1: three, etc.
    [Unreconciled, Internal] = array:get(Index, Old),
    New = array:set(InternalNum -2,
                    case Reconciled of
                        true -> [Unreconciled, Internal + 1];
                        _ -> [Unreconciled + 1, Internal + 1]
                    end,
                    Old),
    State2 = State#state{unreconciled_get = New},
    {noreply, State2}.

add_bytes_read(Data, State) ->
    Bytes = lists:sum([byte_size(D#data.value) || D <- Data]),
    State2 = State#state{bytes_read = State#state.bytes_read + Bytes},
    {noreply, State2}.

add_bytes_write(Data, State) ->
    Bytes = byte_size(Data#data.value),
    State2 = State#state{bytes_write = State#state.bytes_write + Bytes},
    {noreply, State2}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(all, _From, State) ->
    all(State).
handle_cast(incr_cmd_get, State) ->
    incr_cmd_get(State);
handle_cast(incr_cmd_set, State) ->
    incr_cmd_set(State);
handle_cast({add_bytes_read, Data}, State) ->
    add_bytes_read(Data, State);
handle_cast({add_bytes_write, Data}, State) ->
    add_bytes_write(Data, State);
handle_cast({incr_unreconciled_get, {InternalNum, Reconciled}}, State) ->
    incr_unreconciled_get(InternalNum, Reconciled, State).
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
all() ->
    gen_server:call(?SERVER, all).
incr_cmd_get() ->
    gen_server:cast(?SERVER, incr_cmd_get).
incr_cmd_set() ->
    gen_server:cast(?SERVER, incr_cmd_set).
add_bytes_read(Data) ->
    gen_server:cast(?SERVER, {add_bytes_read, Data}).
add_bytes_write(Data) ->
    gen_server:cast(?SERVER, {add_bytes_write, Data}).
incr_unreconciled_get(Data) ->
    gen_server:cast(?SERVER, {incr_unreconciled_get, Data}).
