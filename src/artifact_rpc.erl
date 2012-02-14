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

-module(artifact_rpc).
-behaviour(artifact_tcp_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3]).
-export([
    ok/2,
    node_info/2, node_list/2,
    list/3, get/3, put/3, delete/3,
    check_node/3, route/3
]).

-include("artifact.hrl").

-record(state, {node_info}).

start_link() ->
    artifact_tcp_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        [],
        #tcp_server_option{
            listen = [binary, {packet, 4}, {active, true}, {reuseaddr, true}],
            port          = artifact_config:get(rpc_port),
            max_processes = artifact_config:get(rpc_max_processes)
        }
    ).

stop() -> artifact_tcp_server:stop(?MODULE).

init(_Args) ->
    {ok, #state{node_info = artifact_config:node_info()}}.

handle_call(Socket, Data, State) ->
    dispatch(Socket, binary_to_term(Data), State).

dispatch(_Socket, ok, State) ->
    reply(ok, State);

dispatch(_Socket, node_info, State) ->
    reply(State#state.node_info, State);

dispatch(_Socket, node_list, State) ->
    reply(artifact_hash:node_list(), State);

dispatch(_Socket, {list, Bucket}, State) ->
    reply(artifact_store:list(Bucket), State);

dispatch(_Socket, {get, Data}, State) ->
    reply(artifact_store:get(Data), State);

dispatch(_Socket, {put, Data}, State) when is_record(Data, data)->
    reply(artifact_store:put(Data), State);

dispatch(_Socket, {delete, Data}, State) ->
    reply(artifact_store:delete(Data), State);

dispatch(_Socket, {check_node, Node}, State) ->
    reply(artifact_membership:check_node(Node), State);

dispatch(_Socket, {route, SrcNode, Request}, State) ->
    reply(artifact_coordinator:route(SrcNode, Request), State);

dispatch(_Socket, _Unknown, State) ->
    reply({error, enotsup}, State).

reply(Data, State) ->
    {reply, term_to_binary(Data), State}.

recv_response(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            {ok, binary_to_term(Bin)};
        {tcp_closed, Socket} ->
            {error, econnreset};
        {error, Reason} ->
            {error, Reason}

        %% Don't place Other alternative here.  This is to avoid to catch event
        %% messages, '$gen_event' or something like that.  Remember that this
        %% function can be called from gen_fsm/gen_event.

    after ?TIMEOUT ->
            {error, timeout}
    end.

do_request(Node, Message) ->
    case artifact_connection:lease(Node, self()) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, term_to_binary(Message)) of
                ok ->
                    case recv_response(Socket) of
                        {ok, Result} ->
                            artifact_connection:return(Socket),
                            {ok, Result};
                        {error, Reason} ->
                            artifact_connection:close(Socket),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    artifact_connection:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

request(Node, Message) ->
    case do_request(Node, Message) of
        {ok, Result} ->
            Result;
        {error, Reason} ->
            ?warning(io_lib:format("request(~p, ~p): ~p",
                                   [Node, Message, {error, Reason}])),
%            artifact_membership:check_node(Node),
            {error, Reason}
    end.

ok(DstNode, SrcNode) ->
    case DstNode =:= SrcNode of
        true -> ok;
        _    -> request(DstNode, ok)
    end.

node_info(DstNode, SrcNode) ->
    case DstNode =:= SrcNode of
        true -> artifact_config:node_info();
        _    -> request(DstNode, node_info)
    end.

node_list(DstNode, SrcNode) ->
    case DstNode =:= SrcNode of
        true -> artifact_hash:node_list();
        _    -> request(DstNode, node_list)
    end.

list(DstNode, SrcNode, Bucket) ->
    case DstNode =:= SrcNode of
        true -> artifact_store:list(Bucket);
        _    -> request(DstNode, {list, Bucket})
    end.

get(DstNode, SrcNode, Data) ->
    case DstNode =:= SrcNode of
        true -> artifact_store:get(Data);
        _    -> request(DstNode, {get, Data})
    end.

put(DstNode, SrcNode, Data) ->
    case DstNode =:= SrcNode of
        true -> artifact_store:put(Data);
        _    -> request(DstNode, {put, Data})
    end.

delete(DstNode, SrcNode, Data) ->
    case DstNode =:= SrcNode of
        true -> artifact_store:delete(Data);
        _    -> request(DstNode, {delete, Data})
    end.

check_node(DstNode, SrcNode, Node) ->
    case DstNode =:= SrcNode of
        true -> artifact_membership:check_node(Node);
        _    -> request(DstNode, {check_node, Node})
    end.

route(DstNode, SrcNode, Request) ->
    case DstNode =:= SrcNode of
        true -> {error, ewouldblock};
        _    -> request(DstNode, {route, DstNode, Request})
    end.
