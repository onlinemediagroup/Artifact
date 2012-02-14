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

-module(artifact_membership).
-behaviour(gen_fsm).

-export([start_link/0, stop/0]).
-export([check_node/1]).
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

do_check_node({IpAddr, Port}, State) ->
    {AvailableNodes, DownNodes} = retrieve_node_list({IpAddr, Port}, State),
    {ok, ReplacedBuckets} = artifact_hash:update_nodes(AvailableNodes, DownNodes),
    sync_buckets(ReplacedBuckets).

retrieve_node_list(Node, State) ->
    case artifact_rpc:node_list(Node, State#state.node) of
        {ok, RemoteNodeList} ->
            {ok, LocalNodeList} = artifact_hash:node_list(),
            NewNodes = RemoteNodeList -- LocalNodeList,
            OldNodes = LocalNodeList  -- RemoteNodeList,
            Nodes = NewNodes ++ OldNodes,
            ping_nodes(Nodes -- [State#state.node], [], [], State);
        {error, Reason} ->
            ?warning(io_lib:format("retrieve_node_list/1: ~p", [{error, Reason}])),
            {[], [Node]}
    end.

ping_nodes([], AvailableNodes, DownNodes, _State) ->
    {AvailableNodes, DownNodes};
ping_nodes([Node|Nodes], AvailableNodes, DownNodes, State) ->
    case artifact_rpc:node_info(Node, State#state.node) of
        {ok, Node2, Info} ->
            ping_nodes(Nodes, [{Node2, Info}|AvailableNodes], DownNodes, State);
        {error, Reason} ->
            ?warning(io_lib:format("ping_nodes/3: ~p", [{error, Reason}])),
            ping_nodes(Nodes, AvailableNodes, [Node|DownNodes], State)
    end.

sync_buckets([]) ->
    ok;
sync_buckets([{Bucket, NewReplica, OldReplica}|ReplacedBuckets]) ->
    case {NewReplica, OldReplica} of
        {NewReplica, undefined } -> artifact_sync:update_bucket(Bucket);
        {undefined,  OldReplica} -> artifact_sync:delete_bucket(Bucket);
        _                        -> nop
    end,
    sync_buckets(ReplacedBuckets).

ready({check_node, Node}, State) ->
    do_check_node(Node, State),
    {next_state, ready, State, State#state.interval};
ready(timeout, State) ->
    case artifact_hash:choose_node_randomly() of
        {node, Node} -> do_check_node(Node, State);
        _            -> nop
    end,
    {next_state, ready, State, State#state.interval}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {next_state, ready, StateData, _Interval = 1000}.
handle_info(_Info, _StateName, StateData) ->
    {next_state, ready, StateData, _Interval = 1000}.
code_change(_OldVsn, _StateName, StateData, _Extra) ->
    {ok, ready, StateData}.

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).
check_node(Node) ->
    gen_fsm:send_event(?SERVER, {check_node, Node}).
