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

-module(artifact_hash).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([
    update_nodes/2, find_bucket/1, find_nodes/1, replica_index/1,
    choose_node_randomly/0, choose_bucket_randomly/0,
    node_info/1, node_info/0, node_list/0, virtual_node_list/0, 
    bucket_list/0, buckets/0
]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("artifact.hrl").

-define(SERVER, ?MODULE).
-define(HASH_LEN, 32).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).

init(_Args) ->
    ets:new(node_list, [set, private, named_table]),
    ets:new(virtual_node_list, [ordered_set, private, named_table]),
    ets:new(buckets, [set, private, named_table]),

    {ok, LocalNode, Info} = artifact_config:node_info(),
    update_nodes([{LocalNode, Info}], [], _State = []),

    {ok, _State = []}.

terminate(_Reason, _State) ->
    ets:delete(node_list),
    ets:delete(virtual_node_list),
    ets:delete(buckets),
    ok.


% "It's a p6, triple the speed of the pentium"
hash(Key) ->
    <<HashedKey:?HASH_LEN/integer, _/binary>> = erlang:md5(Key),
    HashedKey.
hash({{N1,N2,N3,N4}, Port}, VirtualNode) ->
    <<HashedKey:?HASH_LEN/integer, _/binary>> =
        erlang:md5(<<N1,N2,N3,N4,Port:16,VirtualNode:16>>),
    HashedKey.

bucket_range(BucketNum) ->
    trunc( math:pow(2, ?HASH_LEN) / BucketNum ).

search_bucket_nodes(_HashedKey, _N, 0, Nodes) ->
    {ok, lists:reverse(Nodes)};
search_bucket_nodes(HashedKey, N, I, Nodes) ->
    HashedNode =
        case ets:next(virtual_node_list, HashedKey) of
            '$end_of_table' -> ets:first(virtual_node_list);
            Other           -> Other
        end,
    [{_HashedNode, Node}|_] = ets:lookup(virtual_node_list, HashedNode),
    Nodes2 =
        case lists:member(Node, Nodes) of
            true -> Nodes;
            _    -> [Node|Nodes]
        end,
    case length(Nodes2) of
        N -> {ok, lists:reverse(Nodes2)};
        _ -> search_bucket_nodes(HashedNode, N, I-1, Nodes2)
    end.

lists_index(_Elem, [], _I) ->
    undefined;
lists_index(Elem, [Head|Rest], I) ->
    case Elem =:= Head of
        true -> I;
        _    -> lists_index(Elem, Rest, I+1)
    end.
lists_index(Elem, List) ->
    lists_index(Elem, List, 1).

update_buckets(-1 = _Bucket, _LocalNode, _BucketRange, _N, _MaxSearch,
               ReplacedBuckets) ->
    {ok, ReplacedBuckets};
update_buckets(Bucket, LocalNode, BucketRange, N, MaxSearch,
               ReplacedBuckets) ->
    {ok, NewNodes} =
        search_bucket_nodes(Bucket * BucketRange, N, MaxSearch, []),
    case ets:lookup(buckets, Bucket) of
        [{Bucket, NewNodes}] ->
            update_buckets(Bucket-1, LocalNode, BucketRange, N, MaxSearch,
                           ReplacedBuckets);
        OldBucket ->
            ets:insert(buckets, {Bucket, NewNodes}),
            NewReplica = lists_index(LocalNode, NewNodes),
            OldReplica = 
                case OldBucket of
                    [{Bucket, OldNodes}] -> lists_index(LocalNode, OldNodes);
                    []                   -> undefined
                end,
            ReplacedBuckets2 =
                case {NewReplica, OldReplica} of
                    {Replica, Replica} ->
                        ReplacedBuckets;
                    _ ->
                        [{Bucket, NewReplica, OldReplica}|ReplacedBuckets]
                end,
            update_buckets(Bucket-1, LocalNode, BucketRange, N, MaxSearch,
                           ReplacedBuckets2)
    end.

update_buckets() ->
    [LocalNode, {N,_R,_W}, BucketNum] =
        artifact_config:get([node, quorum, buckets]),
    BucketRange = bucket_range(BucketNum),
    NodeNum = proplists:get_value(size, ets:info(node_list)),

    %% Don't search other nodes to fill a bucket when NodeNum is 1, since
    %% they are never found.
    MaxSearch =
        case NodeNum of
            1 -> 1;
            _ -> proplists:get_value(size, ets:info(virtual_node_list))
        end,

    update_buckets(BucketNum-1, LocalNode, BucketRange, N, MaxSearch, []).

add_nodes([]) ->
    ok;
add_nodes([{Node, Info}|Rest]) ->
    case ets:lookup(node_list, Node) of
        [{Node, _Info}|_] -> ok;
        [] ->
            ets:insert(node_list, {Node, Info}),
            VirtualNodeNum = proplists:get_value(virtual_nodes, Info),
            lists:foreach(
              fun(VirtualNode) ->
                      HashedKey = hash(Node, VirtualNode),
                      ets:insert(virtual_node_list, {HashedKey, Node})
              end,
              lists:seq(1, VirtualNodeNum)
             )
    end,
    add_nodes(Rest).

remove_nodes([]) ->
    ok;
remove_nodes([Node|Rest]) ->
    case ets:lookup(node_list, Node) of
        [{Node, Info}|_] ->
            ets:delete(node_list, Node),
            VirtualNodeNum = proplists:get_value(virtual_nodes, Info),
            lists:foreach(
              fun(VirtualNode) ->
                      HashedKey = hash(Node, VirtualNode),
                      ets:delete(virtual_node_list, HashedKey)
              end,
              lists:seq(1, VirtualNodeNum)
             );
        [] -> ok
    end,
    remove_nodes(Rest).

update_nodes(NodesToAdd, NodesToRemove, State) ->
    LocalNode = artifact_config:get(node),
    Reply =
        case {NodesToAdd, NodesToRemove -- [LocalNode]} of
            {[], []} ->
                {ok, []};
            _ ->
                ?info({update, NodesToAdd, NodesToRemove}),
                add_nodes(NodesToAdd),
                remove_nodes(NodesToRemove),
                update_buckets()
        end,
    {reply, Reply, State}.

do_find_bucket(Bucket, BucketNum) when is_integer(Bucket) ->
    Bucket rem BucketNum;
do_find_bucket(Key, BucketNum) ->
    hash(Key) div bucket_range(BucketNum).

find_bucket(KeyOrBucket, State) ->
    BucketNum = artifact_config:get(buckets),
    {reply, {ok, do_find_bucket(KeyOrBucket, BucketNum)}, State}.

find_nodes(KeyOrBucket, State) ->
    BucketNum = artifact_config:get(buckets),
    Bucket = do_find_bucket(KeyOrBucket, BucketNum),
    [{Bucket, Nodes}|_] = ets:lookup(buckets, Bucket),
    {reply, {ok, Nodes}, State}.

replica_index(KeyOrBucket, State) ->
    LocalNode = artifact_config:get(node),
    {reply, {ok, Nodes}, State2} = find_nodes(KeyOrBucket, State),
    ReplicaIndex = lists_index(LocalNode, Nodes),
    {reply, {ok, ReplicaIndex}, State2}.

choose_node_randomly(State) ->
    {{N1,N2,N3,N4}, Port} = artifact_config:get(node),
    Head = {'$1', '_'},
    Cond = [{'=/=', '$1', {{{{N1,N2,N3,N4}}, Port}}}], %% double tuple paranthesis
    Body = ['$1'],
    Nodes = ets:select(node_list, [{Head, Cond, Body}]),
    Len = length(Nodes),
    case Len of
        0 -> {reply, undefined, State};
        _ -> {reply, {node, lists:nth(random:uniform(Len), Nodes)}, State}
    end.

inversed_buckets(_Node, -1 = _Bucket, Buckets) ->
    Buckets;
inversed_buckets(Node, Bucket, Buckets) ->
    [{Bucket, Nodes}|_] = ets:lookup(buckets, Bucket),
    case lists:member(Node, Nodes) of
        true -> inversed_buckets(Node, Bucket-1, [Bucket|Buckets]);
        _    -> inversed_buckets(Node, Bucket-1, Buckets)
    end.

inversed_buckets(Node) ->
    BucketNum = artifact_config:get(buckets),
    inversed_buckets(Node, BucketNum-1, []).

choose_bucket_randomly(State) ->
    LocalNode = artifact_config:get(node),
    Buckets = inversed_buckets(LocalNode),
    Len = length(Buckets),
    case Len of
        0 -> {reply, undefined, State};
        _ -> {reply, {ok, lists:nth(random:uniform(Len), Buckets)}, State}
    end.

do_node_info(Node, State) ->
    Head = {Node, '$2'},
    Cond = [],
    Body = ['$2'],
    [Info] = ets:select(node_list, [{Head, Cond, Body}]),
    {reply, {ok, Node, Info}, State}.

do_node_info(State) ->
    LocalNode = artifact_config:get(node),
    do_node_info(LocalNode, State).

node_list(State) ->
    NodeList = ets:tab2list(node_list),
    NodeList2 = lists:map(fun({Node, _Info}) -> Node end, NodeList),
    {reply, {ok, NodeList2}, State}.

virtual_node_list(State) ->
    VirtualNodeList = ets:tab2list(virtual_node_list),
    {reply, {ok, VirtualNodeList}, State}.

bucket_list(State) ->
    Buckets = ets:tab2list(buckets),
    {reply, {ok, lists:sort(Buckets)}, State}.

buckets(State) ->
    LocalNode = artifact_config:get(node),
    Buckets =
        lists:filter(
          fun(B) -> lists:member(LocalNode, element(2, B)) end,
          ets:tab2list(buckets)
         ),
    Buckets2 = [element(1, B) || B <- Buckets],
    {reply, {ok, lists:sort(Buckets2)}, State}.

% Is it possible to reduce the handling of calls determinstically without
% calling a disparate set of methods?

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({update_nodes, NodesToAdd, NodesToRemove}, _From, State) ->
    update_nodes(NodesToAdd, NodesToRemove, State);
handle_call({find_bucket, KeyOrBucket}, _From, State) ->
    find_bucket(KeyOrBucket, State);
handle_call({find_nodes, KeyOrBucket}, _From, State) ->
    find_nodes(KeyOrBucket, State);
handle_call({replica_index, KeyOrBucket}, _From, State) ->
    replica_index(KeyOrBucket, State);
handle_call(choose_node_randomly, _From, State) ->
    choose_node_randomly(State);
handle_call(choose_bucket_randomly, _From, State) ->
    choose_bucket_randomly(State);
handle_call({node_info, Node}, _From, State) ->
    do_node_info(Node, State);
handle_call(node_info, _From, State) ->
    do_node_info(State);
handle_call(node_list, _From, State) ->
    node_list(State);
handle_call(virtual_node_list, _From, State) ->
    virtual_node_list(State);
handle_call(bucket_list, _From, State) ->
    bucket_list(State);
handle_call(buckets, _From, State) ->
    buckets(State).
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
update_nodes(NodesToAdd, NodesToRemove) ->
    gen_server:call(?SERVER, {update_nodes, NodesToAdd, NodesToRemove}).
find_bucket(KeyOrBucket) ->
    gen_server:call(?SERVER, {find_bucket, KeyOrBucket}).
replica_index(KeyOrBucket) ->
    gen_server:call(?SERVER, {replica_index, KeyOrBucket}).
find_nodes(KeyOrBucket) ->
    gen_server:call(?SERVER, {find_nodes, KeyOrBucket}).
choose_node_randomly() ->
    gen_server:call(?SERVER, choose_node_randomly).
choose_bucket_randomly() ->
    gen_server:call(?SERVER, choose_bucket_randomly).
node_info() ->
    gen_server:call(?SERVER, node_info).
node_info(Node) ->
    gen_server:call(?SERVER, {node_info, Node}).
node_list() ->
    gen_server:call(?SERVER, node_list).
virtual_node_list() ->
    gen_server:call(?SERVER, virtual_node_list).
bucket_list() ->
    gen_server:call(?SERVER, bucket_list).
buckets() ->
    gen_server:call(?SERVER, buckets).
