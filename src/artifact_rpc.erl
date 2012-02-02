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
    node_info/1, node_list/1,
    list/2, get/2, put/2, delete/2,
    check_node/2, route/2
]).

-include("artifact.hrl").

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

init(_Args) -> {ok, {}}.

handle_call(Socket, Data, State) ->
    dispatch(Socket, binary_to_term(Data), State).

dispatch(_Socket, node_info, State) ->
    reply(artifact_config:node_info(), State);

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

dispatch(_Socket, {route, Request}, State) ->
    reply(artifact_coordinator:route(Request), State);

dispatch(_Socket, _Unknown, State) ->
    reply({error, enotsup}, State).

reply(Data, State) ->
    {reply, term_to_binary(Data), State}.

recv_response(ApiSocket) ->
    receive
        {tcp, ApiSocket, Bin} ->
            {ok, binary_to_term(Bin)};
        {tcp_closed, ApiSocket} ->
            {error, econnreset};
        {error, Reason} ->
            {error, Reason}

        % Don't place Other alternative here.  This is to avoid to catch event
        % messages, '$gen_event' or something like that.  Remember that this
        % function can be called from gen_fsm/gen_event.

    after ?TIMEOUT ->
            {error, timeout}
    end.

do_request(Node, Message) ->
    case artifact_connection:lease(Node, self()) of
        {ok, ApiSocket} ->
            case gen_tcp:send(ApiSocket, term_to_binary(Message)) of
                ok ->
                    case recv_response(ApiSocket) of
                        {ok, Result} ->
                            artifact_connection:return(ApiSocket),
                            {ok, Result};
                        {error, Reason} ->
                            artifact_connection:close(ApiSocket),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    artifact_connection:close(ApiSocket),
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

is_local_node(Node) ->
    LocalNode = artifact_config:get(node),
    Node =:= LocalNode.

node_info(Node) ->
    case is_local_node(Node) of
        true -> artifact_config:node_info();
        _    -> request(Node, node_info)
    end.

node_list(Node) ->
    case is_local_node(Node) of
        true -> artifact_hash:node_list();
        _    -> request(Node, node_list)
    end.

list(Node, Bucket) ->
    case is_local_node(Node) of
        true -> artifact_store:list(Bucket);
        _    -> request(Node, {list, Bucket})
    end.

get(Node, Data) ->
    case is_local_node(Node) of
        true -> artifact_store:get(Data);
        _    -> request(Node, {get, Data})
    end.

put(Node, Data) ->
    case is_local_node(Node) of
        true -> artifact_store:put(Data);
        _    -> request(Node, {put, Data})
    end.

delete(Node, Data) ->
    case is_local_node(Node) of
        true -> artifact_store:delete(Data);
        _    -> request(Node, {delete, Data})
    end.

check_node(Node, Node2) ->
    case is_local_node(Node) of
        true -> artifact_membership:check_node(Node2);
        _    -> request(Node, {check_node, Node2})
    end.

route(Node, Request) ->
    case is_local_node(Node) of
        true -> {error, ewouldblock};
        _    -> request(Node, {route, Request})
    end.
