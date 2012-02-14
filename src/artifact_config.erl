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

-module(artifact_config).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([get/1, node_info/0]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).
-export([get_env/0]).

-include("artifact.hrl").

-record(state, {configs}).

-define(SERVER, ?MODULE).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, _Opts = []).

init(Configs) ->
    {ok, #state{
       configs = fixup(compat(Configs))
      }}.

terminate(_Reason, _State) ->
    ok.

compat(Configs) ->
    NewConfigs = 
        lists:map(
          fun(K) -> compat(K, Configs) end,
          [buckets, virtual_nodes, quorum, dets_tables]
         ),
    lists:flatten([NewConfigs|Configs]).

compat(buckets, Configs) ->
    case proplists:get_value(number_of_buckets, Configs) of
        undefined -> [];
        BucketNum ->
            [{buckets, BucketNum}]
    end;
compat(virtual_nodes, Configs) ->
    case proplists:get_value(number_of_virtual_nodes, Configs) of
        undefined -> [];
        VirtualNodeNum ->
            [{virtual_nodes, VirtualNodeNum}]
    end;
compat(quorum, Configs) ->
    case proplists:get_value(n, Configs) of
        undefined -> [];
        N ->
            R = proplists:get_value(r, Configs),
            W = proplists:get_value(w, Configs),
            [{quorum, {N,R,W}}]
    end;
compat(dets_tables, Configs) ->
    case proplists:get_value(number_of_tables, Configs) of
        undefined -> [];
        TableNum ->
            [{dets_tables, TableNum}]
    end.

fixup(Configs) ->
    NewConfigs =
        lists:map(
          fun(K) -> fixup(K, Configs) end,
          [node, quorum, buckets]
         ),
    lists:flatten([NewConfigs|Configs]).

fixup(node, Configs) ->
    Hostname =
        case proplists:get_value(hostname, Configs) of
            undefined -> {ok, H} = inet:gethostname(), H;
            H         -> H
        end,
    {ok, IpAddr} = inet:getaddr(Hostname, inet),
    Port = proplists:get_value(rpc_port, Configs),
    [{node, {IpAddr, Port}}];
fixup(quorum, Configs) ->
    {N,R,W} = proplists:get_value(quorum, Configs),
    if
        R + W > N -> ok;
        true -> exit("To reach quorum, there must be some R + W > N")
    end,
    if
        W > N/2 -> ok;
        true -> exit("to meet quorum, there must be some W > N/2")
    end,
    [];
fixup(buckets, Configs) ->
    %% the number of buckets is 2^N greater than the specified quorum
    BucketNum = proplists:get_value(buckets, Configs),
    Exponent = round( math:log(BucketNum) / math:log(2) ),
    AlignedBucketNum = trunc( math:pow(2, Exponent) ),
    [{buckets, AlignedBucketNum}].
             
do_get(Key, State) ->
    proplists:get_value(Key, State#state.configs).

do_get([], ValueList, _State) ->
    lists:reverse(ValueList);
do_get([Key|Rest], ValueList, State) ->
    do_get(Rest, [do_get(Key, State)|ValueList], State).

get(KeyList, State) when is_list(KeyList)->
    {reply, do_get(KeyList, [], State), State};
get(Key, State) ->
    {reply, do_get(Key, State), State}.

node_info(State) ->
    [LocalNode, VirtualNodeNum] = do_get([node, virtual_nodes], [], State),
    Info = [{virtual_nodes, VirtualNodeNum}],
    {reply, {ok, LocalNode, Info}, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({get, Key}, _From, State) ->
    get(Key, State);
handle_call(node_info, _From, State) ->
    node_info(State).
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).
node_info() ->
    gen_server:call(?SERVER, node_info).

%% initialized by start()
get_env() ->
    lists:flatten(
      lists:map(
        fun(K) -> 
                case application:get_env(artifact, K) of
                    undefined -> [];
                    {ok, V}   -> [{K, V}]
                end
        end,
        [logfile, hostname,
         rpc_port, rpc_max_processes,
         memcache_port, memcache_max_processes,
         max_connections,
         quorum,
         buckets, virtual_nodes,
         store, dets_dir, dets_tables,
         sync_interval, membership_interval]
       )
     ).
