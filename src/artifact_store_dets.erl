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

-module(artifact_store_dets).
-behaviour(gen_server).

-export([start_link/1]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("artifact.hrl").

-record(state, {tables, table_list}).

start_link(Server) ->
    gen_server:start_link({local, Server}, ?MODULE, [], _Opts = []).

init(_Args) ->
    [Dir, TableNum] =artifact_config:get([dets_dir, dets_tables]),
    TableList =
        lists:map(
          fun(I) ->
                  Name = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(I)),
                  File = Dir ++ "/" ++ integer_to_list(I),
                  case dets:open_file(Name, [{type, bag}, {keypos, 2}, {file, File}]) of
                      {ok, Table} -> {I, Table};
                      {error, Reason} -> ?info(Reason),
                                         exit(Reason)
                  end
          end,
          lists:seq(1, TableNum)
         ),
    {ok, #state{tables = TableNum, table_list = TableList}}.

terminate(_Reason, State) ->
    lists:foreach(
      fun({_I, Table}) -> dets:close(Table) end,
      State#state.table_list
     ),
    ok.

bucket_to_table(Bucket, State) ->
    I = Bucket rem State#state.tables + 1,
    proplists:get_value(I, State#state.table_list).

do_list(Bucket, State) ->
    Table = bucket_to_table(Bucket, State),
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
    KeyList = dets:select(Table, [{Head, Cond, Body}]),
    {reply, {ok, KeyList}, State}.

%% No specific reason for this code change, just want to look like
%% standard OTP code

do_get(#data{key=Key, bucket=Bucket} = _Data, State) ->
    Table = bucket_to_table(Bucket, State),
    case dets:lookup(Table, Key) of
        []      -> {reply, undefined, State};
        StoredDataList -> {reply, StoredDataList, State}
    end.

do_put(Data, State) when is_record(Data, data) ->
    Table = bucket_to_table(Data#data.bucket, State),
    insert_and_remove(Table, Data, dets:lookup(Table, Data#data.key)),
    {reply, ok, State}.

insert_and_remove(Table, Data, StoredDataList) ->
    dets:insert(Table, Data),
    remove_descend_data(Table, Data#data.vector_clocks, StoredDataList),
    dets:sync(Table).

remove_descend_data(_Table, _Vc, []) ->
    ok;
remove_descend_data(Table, Vc, [StoredData|Rest]) ->
    case vclock:descends(Vc, StoredData#data.vector_clocks) of
        true -> dets:delete_object(Table, StoredData);
        _ -> nop
    end,
    remove_descend_data(Table, Vc, Rest).

do_delete(#data{key=Key, bucket=Bucket} = _Data, State) ->
    Table = bucket_to_table(Bucket, State),
    case dets:lookup(Table, Key) of
        [] ->
            {reply, undefined, State};
        _StoredDataList ->
            dets:delete(Table, Key),
            dets:sync(Table),
            {reply, ok, State}
    end.

info(Name, State) ->
    Values =
        lists:map(
          fun(I) ->
                  T = proplists:get_value(I, State#state.table_list),
                  case Name of
                      bytes -> dets:info(T, file_size);
                      size  -> dets:info(T, size);
                      _     -> undefined
                  end
          end,
          lists:seq(1, State#state.tables)
         ),
    {reply, lists:sum(Values), State}.

%% "Ilya, thats a Giraffe's name!"

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
