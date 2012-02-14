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

-module(artifact_store_ets).
-behaviour(gen_server).

-export([start_link/1]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("artifact.hrl").

start_link(Server) ->
    gen_server:start_link({local, Server}, ?MODULE, [], _Opts = []).

init(_Args) ->
    ets:new(?MODULE, [bag, private, named_table, {keypos, 2}]),
    {ok, []}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

do_list(Bucket, State) ->
    Head = #data{
        key           = '$1',
        bucket        = Bucket,
        last_modified = '$2',
        vector_clocks = '$3',
        checksum      = '$4',
        flags         = '_',
        value         = '_'
    },
    Cond = [],
    Body = [{#data{
        key           = '$1',
        bucket        = Bucket,
        last_modified = '$2',
        vector_clocks = '$3',
        checksum      = '$4'
    }}],
    KeyList = ets:select(?MODULE, [{Head, Cond, Body}]),
    {reply, {ok, KeyList}, State}.

do_get(#data{key=Key} = _Data, State) ->
    case ets:lookup(?MODULE, Key) of
        []      -> {reply, undefined, State};
        StoredDataList    -> {reply, StoredDataList, State}
    end.

do_put(Data, State) when is_record(Data, data) ->
    insert_and_remove(Data, ets:lookup(?MODULE, Data#data.key)),
    {reply, ok, State}.

insert_and_remove(Data, StoredDataList) ->
    ets:insert(?MODULE, Data),
    remove_descend_data(Data#data.vector_clocks, StoredDataList).

remove_descend_data(_Vc, []) ->
    ok;
remove_descend_data(Vc, [StoredData|Rest]) ->
    case vclock:descends(Vc, StoredData#data.vector_clocks) of
        true -> ets:delete_object(?MODULE, StoredData);
        _ -> nop
    end,
    remove_descend_data(Vc, Rest).

do_delete(#data{key=Key} = _Data, State) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            {reply, undefined, State};
        _StoredDataList ->
            ets:delete(?MODULE, Key),
            {reply, ok, State}
    end.


info(Name, State) ->
    Value =
        case Name of
            bytes ->
                %% Some Artifact Internals:
                %% ETS only stores a reference to the binary blob,
                %% estimation is a computationally difficult test so
                %% we use sysinfo to get an estimate
                Ets = erlang:system_info(wordsize) * ets:info(?MODULE, memory),
                Bin = erlang:memory(binary),
                Ets + Bin;
            size ->
                ets:info(?MODULE, size);
            _ ->
                undefined
        end,
    {reply, Value, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({list, Bucket}, _From, State) ->
    do_list(Bucket, State);
handle_call({get, Data}, _From, State) ->
    do_get(Data, State);
handle_call({put, Data}, _From, State) ->
    do_put(Data, State);
handle_call({delete, Data}, _From, State) ->
    do_delete(Data, State);
handle_call({info, Name}, _From, State) ->
    info(Name, State).
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
