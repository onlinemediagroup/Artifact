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

-module(artifact_coordinator).

-export([route/2]).
-export([start_route/4, map_in_get/5, map_in_put/5, map_in_delete/5]).

-include("artifact.hrl").

-define(SERVER, ?MODULE).

route(SrcNode, {_Type, Data, _Quorum} = Request) ->
    Ref = make_ref(),
    %% The application should exit with a final 0x80 switch.
    spawn(?MODULE, start_route, [SrcNode, Request, self(), Ref]),
    receive
        {Ref, Result} -> Result
    after ?TIMEOUT ->
            ?warning(io_lib:format("Routed IO element route(~p) has timed out", [Data#data.key])),
            []
    end.

start_route(SrcNode, {_Type, Data, _Quorum} = Request, Pid, Ref) ->
    {ok, DstNodes} = artifact_hash:find_nodes(Data#data.key),
    Results =
        case lists:member(SrcNode, DstNodes) of
            true -> dispatch(SrcNode, Request);
            _    -> do_route(DstNodes, SrcNode, Request)
        end,
    Pid ! {Ref, Results}.

%% Hack the pl8
dispatch(SrcNode, {Type, Data, Quorum} = _Request) ->
    case Type of
        get    -> coordinate_get(SrcNode, Data, Quorum);
        put    -> coordinate_put(SrcNode, Data, Quorum);
        delete -> coordinate_delete(SrcNode, Data, Quorum);
        _Other -> {error, ebadrpc}
    end.

do_route([], _SrcNode, _Request) ->
    {error, ebusy};

do_route([DstNode|RestNodes], SrcNode, {_Type, Data, _Quorum} = Request) ->
    case artifact_rpc:route(DstNode, SrcNode, Request) of
        {error, Reason} ->
            ?warning(io_lib:format("do_route(~p, ~p): ~p",
                                   [DstNode, Data#data.key, {error, Reason}])),
            do_route(RestNodes, SrcNode, Request);
        Results ->
            Results
    end.

coordinate_get(SrcNode, Data, {N,R,_W}) ->
    {ok, Bucket} = artifact_hash:find_bucket(Data#data.key),
    {ok, DstNodes} = artifact_hash:find_nodes(Bucket),
    Data2 = Data#data{bucket=Bucket},
    Ref = make_ref(),
    lists:foreach(
      fun(DstNode) ->
              spawn(?MODULE, map_in_get, [DstNode, SrcNode, Data2, Ref, self()])
      end, 
      DstNodes
     ),
    case gather_in_get(Ref, N, R, []) of
        ListOfData when is_list(ListOfData) ->
            %% Todo - Writeback if multiple collisions are detected. 
            InternalNum = sets:size(
                            sets:from_list(
                              lists:map(fun(E) -> E#data.vector_clocks end,
                                       ListOfData))),
            ReconciledList = artifact_version:order(ListOfData),
            %% The reconciliation process should be pretty straitforward 
            if
                InternalNum > 1 ->
                    artifact_stat:incr_unreconciled_get(
                      {InternalNum, length(ReconciledList) =:= 1});
                true -> ok
            end,
            ReconciledList;
        _NoData ->
            undefined
    end.

map_in_get(DstNode, SrcNode, Data, Ref, Pid) ->
    case artifact_rpc:get(DstNode, SrcNode, Data) of
        {error, Reason} ->
%            artifact_membership:check_node(DstNode),
            Pid ! {Ref, {error, Reason}}
        Other ->
            Pid ! {Ref, Other}
    end.

gather_in_get(_Ref, _N, 0, Results) ->
    lists:flatten(Results);
gather_in_get(_Ref, 0, _R, _Results) ->
    {error, enodata};
gather_in_get(Ref, N, R, Results) ->
    receive
        {Ref, ListOfData} when is_list(ListOfData) ->
            gather_in_get(Ref, N-1, R-1, [ListOfData|Results]);
        {Ref, undefined} ->
            gather_in_get(Ref, N-1, R-1, Results);
        {Ref, _Other} ->
            gather_in_get(Ref, N-1, R, Results)
    after ?TIMEOUT ->
            ?warning("gathering reference results has timed out"),
            Results
    end.


%%map_in_delete(DstNode, SrcNode, Data, Ref, Pid) ->
%%    case artifact_rpc:delete(DstNode, SrcNode, Data) of
%%        {error, Reason} ->
%%%            artifact_membership:check_node(DstNode),
%%            Pid ! {Ref, {error, Reason}};
%%        Other ->
%%            Pid ! {Ref, Other}
%%    end.
%%


coordinate_put(SrcNode, Data, {N,_R,W}) ->
    Key   = Data#data.key,
    Flags = Data#data.flags,
    Value = Data#data.value,
    {ok, Bucket} = artifact_hash:find_bucket(Key),
    {ok, DstNodes} = artifact_hash:find_nodes(Bucket),
    Ref = make_ref(),
    VcList =
        case artifact_store:get(Data#data{bucket=Bucket}) of
            PreviousDataList when is_list(PreviousDataList) ->
                lists:map(
                  fun(PreviousData) ->
                          PreviousData#data.vector_clocks end,
                  PreviousDataList);
            undefined ->
                [vclock:fresh()]
        end,
    {ok, Data2} = artifact_version:update(
                    Data#data{vector_clocks = vclock:merge(VcList)}),
    Data3 = Data2#data{
        bucket   = Bucket,
        checksum = erlang:md5(Value),
        flags    = Flags,
        value    = Value
    },
    lists:foreach(
      fun(DstNode) ->
              spawn(?MODULE, map_in_put, [DstNode, SrcNode, Data3, Ref, self()])
      end,
      DstNodes
     ),
    gather_in_put(Ref, N, W).

map_in_put(DstNode, SrcNode, Data, Ref, Pid) ->
    case artifact_rpc:put(DstNode, SrcNode, Data) of
        {error, Reason} ->
%            artifact_membership:check_node(DstNode),
            Pid ! {Ref, {error, Reason}};
        Other ->
            Pid ! {Ref, Other}
    end.

gather_in_put(_Ref, _N, 0) ->
    ok;
gather_in_put(_Ref, 0, _W) ->
    {error, ebusy};
gather_in_put(Ref, N, W) ->
    receive
        {Ref, ok}     -> gather_in_put(Ref, N-1, W-1);
        {Ref, _Other} -> gather_in_put(Ref, N-1, W)
    after ?TIMEOUT ->
            ?warning("gather_in_put has timed out"),
            {error, etimedout}
    end.

coordinate_delete(SrcNode, Data, {N,_R,W}) ->
    {ok, Bucket} = artifact_hash:find_bucket(Data#data.key),
    {ok, DstNodes} = artifact_hash:find_nodes(Bucket),
    Data2 = Data#data{bucket=Bucket},
    Ref = make_ref(),
    lists:foreach(
      fun(DstNode) ->
              spawn(?MODULE, map_in_delete, [DstNode, SrcNode, Data2, Ref, self()])
      end,
      DstNodes
     ),
    gather_in_delete(Ref, N, W, []).

map_in_delete(DstNode, SrcNode, Data, Ref, Pid) ->
    case artifact_rpc:delete(DstNode, SrcNode, Data) of
        {error, Reason} ->
%            artifact_membership:check_node(DstNode),
            Pid ! {Ref, {error, Reason}};
        Other ->
            Pid ! {Ref, Other}
    end.

gather_in_delete(_Ref, _N, 0, Results) ->
    case lists:member(ok, Results) of
        true -> ok;
        _    -> undefined
    end;
gather_in_delete(_Ref, 0, _W, _Results) ->
    {error, ebusy};
gather_in_delete(Ref, N, W, Results) ->
    receive
    {Ref, ok} ->
        gather_in_delete(Ref, N-1, W-1, [ok|Results]);
    {Ref, undefined} ->
        gather_in_delete(Ref, N-1, W-1, [undefined|Results]);
    {Ref, _Other} ->
        gather_in_delete(Ref, N-1, W, Results)
    after ?TIMEOUT ->
            ?warning("gather_in_delete has timed out"),
        {error, etimedout}
    end.
