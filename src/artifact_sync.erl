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


-module(artifact_sync).
-behaviour(gen_fsm).

-export([start_link/0, stop/0]).
-export([update_bucket/1, delete_bucket/1]).
-export([
    init/1, ready/2, handle_event/3, handle_sync_event/4, handle_info/3,
    terminate/3, code_change/4
]).

-include("artifact.hrl").

-record(state, {node, interval}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).
    
init(_Args) ->
    [LocalNode, Interval] = artifact_config:get([node, sync_interval]),
    {ok, ready, #state{
           node     = LocalNode,
           interval = Interval
          }, Interval}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

do_update_bucket(Bucket, State) when is_record(State, state) ->
    {ok, Nodes} = artifact_hash:find_nodes(Bucket),
    do_update_bucket(Bucket, Nodes -- [State#state.node], State).

do_update_bucket(_Bucket, [], _State) ->
    {error, enodata};
do_update_bucket(Bucket, [Node|Rest], State) ->
    case artifact_rpc:list(Node, State#state.node, Bucket) of
        {ok, KeyList} ->
            retrieve_data(Node, KeyList, State);
        {error, Reason} ->
            ?warning(io_lib:format("do_update_bucket/2: ~p", [{error, Reason}])),
            do_update_bucket(Bucket, Rest, State)
    end.

retrieve_data(_Node, [], _State) ->
    ok;
retrieve_data(Node, [Metadata|Rest], State) ->
    case artifact_store:get(Metadata) of
        DataList when is_list(DataList) ->
            retrieve_data(Node, Rest, State);
        undefined ->
            case artifact_rpc:get(Node, State#state.node, Metadata) of
                RemoteDataList when is_list(RemoteDataList) ->
                    lists:map(fun(RemoteData) ->
                                      artifact_store:put(RemoteData) end,
                              RemoteDataList),
                    retrieve_data(Node, Rest, State);
                undefined ->
                    retrieve_data(Node, Rest, State);
                {error, Reason} ->
                    ?warning(io_lib:format("retrieve_data/2: ~p", [{error, Reason}])),
                    {error, Reason}
            end
    end.

do_delete_bucket([]) ->
    % todo - fix this.
    ok;
do_delete_bucket([Metadata|Rest]) ->
    artifact_store:delete(Metadata),
    do_delete_bucket(Rest);
do_delete_bucket(Bucket) ->
    {ok, KeyList} = artifact_store:list(Bucket),
    do_delete_bucket(KeyList).

ready({update_bucket, Bucket}, State) ->
    do_update_bucket(Bucket, State),
    {next_state, ready, State, State#state.interval};
ready({delete_bucket, Bucket}, State) ->
    do_delete_bucket(Bucket),
    {next_state, ready, State, State#state.interval};
ready(timeout, State) ->
    case artifact_hash:choose_bucket_randomly() of
        {ok, Bucket} -> do_update_bucket(Bucket, State);
        _            -> nop
    end,
    {next_state, ready, State, State#state.interval}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {next_state, wait, StateData, _Interval = 1000}.
handle_info(_Info, _StateName, StateData) ->
    {next_state, ready, StateData, _Interval = 1000}.
code_change(_OldVsn, _StateName, StateData, _Extra) ->
    {ok, ready, StateData}.

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).
update_bucket(Bucket) ->
    gen_fsm:send_event(?SERVER, {update_bucket, Bucket}).
delete_bucket(Bucket) ->
    gen_fsm:send_event(?SERVER, {delete_bucket, Bucket}).
