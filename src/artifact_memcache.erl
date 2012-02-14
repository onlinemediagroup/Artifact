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


-module(artifact_memcache).
-behaviour(artifact_tcp_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3]).  
-include("artifact.hrl").

-record(state, {node, quorum}).

-define(MEMCACHE_TIMEOUT, ?TIMEOUT).

start_link() ->
    artifact_tcp_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        [],
        #tcp_server_option{
            port          = artifact_config:get(memcache_port),
            max_processes = artifact_config:get(memcache_max_processes)
        }
    ).

stop() -> artifact_tcp_server:stop(?MODULE).

init(_Args) ->
    {ok, #state{
       node   = artifact_config:get(node),
       quorum = artifact_config:get(quorum)
      }}.

handle_call(Socket, Data, State) ->
    dispatch(Socket, string:tokens(binary_to_list(Data), " \r\n"), State).

dispatch(_Socket, ["get", Key], State) ->
    do_get(Key, State, false);
dispatch(_Socket, ["gets", Key], State) ->
    do_get(Key, State, true);

dispatch(Socket, ["set", _Key, _Flags, "0", _Bytes] = Data, State) ->
    inet:setopts(Socket, [{packet, raw}]),
    Result = recv_set_data(Socket, Data, State),
    inet:setopts(Socket, [{packet, line}]),
    Result;

dispatch(_Socket, ["set", _Key, _Flags, _Exptime, _Bytes], State) ->
    {reply, <<"client error - exponential time must be zero.\r\n">>, State};

dispatch(_Socket, ["delete", Key], State) ->
    case artifact_coordinator:route(
           State#state.node,
           {delete, #data{key = Key}, State#state.quorum}
          ) of
        ok        -> {reply, <<"Annihilated\r\n">>, State};
        undefined -> {reply, <<"Unfound\r\n">>, State};
        _Other    ->
            send_error_and_close(State, "Failed to annihilate.")
    end;

dispatch(_Socket, ["delete", Key, "0"], State) ->
    dispatch(_Socket, ["delete", Key], State);
dispatch(_Socket, ["delete", _Key, _Time], State) ->
    {reply, <<"client error - Time must be zero.\r\n">>, State};

dispatch(_Socket, ["delete", _Key, _Time, "noreply"], State) ->
    {reply, <<"client error - noreply not supported.\r\n">>, State};

dispatch(_Socket, ["stats"], State) ->
    Response =
        lists:map(
          fun({Name, Value}) ->
                  ["STAT " ++ atom_to_list(Name) ++ " " ++ Value ++ "\r\n"]
          end,
          artifact_stat:all()
         ),
    {reply, [Response|"END\r\n"], State};

dispatch(_Socket, ["version"], State) ->
    Version =
        case application:get_key(artifact, vsn) of
            {ok, V} -> V;
            _       -> "0"
        end,
    {reply, "Version " ++ Version ++ "\r\n", State};

dispatch(_Socket, ["quit"], State) ->
    {close, State};

dispatch(_Socket, _Unknown, State) ->
    {reply, <<"Dispatch error\r\n">>, State}.

do_get(Key, State, WithCasUnique) ->
    case artifact_coordinator:route(
           State#state.node,
           {get, #data{key = Key}, State#state.quorum}
          ) of
        Data when is_list(Data) ->
            {ok, CasUniqueInBinary} = artifact_version:cas_unique(Data),
            Response = get_response(Data, WithCasUnique, CasUniqueInBinary),
            artifact_stat:incr_cmd_get(),
            artifact_stat:add_bytes_read(Data),
            {reply, [Response|"END\r\n"], State};
        undefined ->
            {reply, <<"END\r\n">>, State};
        _Other ->
            send_error_and_close("read failure", State)
    end.

get_response(Data, WithCasUnique, CasUnique) ->
    lists:map(
      fun(Elem) ->
              Key = Elem#data.key,
              Flags = Elem#data.flags,
              Value = Elem#data.value,
              [
               io_lib:format("VALUE ~s ~s ~w", [Key, Flags, byte_size(Value)]),
               case WithCasUnique of
                   true ->
                       io_lib:format(" ~w", [cas_unique(CasUnique)]);
                   _ -> []
               end,
               "\r\n", Value, "\r\n"]
      end, Data).

recv_set_data(Socket, ["set", Key, Flags, "0", Bytes], State) ->
    case gen_tcp:recv(Socket, list_to_integer(Bytes), ?MEMCACHE_TIMEOUT) of
        {ok, Value} ->
            gen_tcp:recv(Socket, 2, ?MEMCACHE_TIMEOUT),
            case artifact_coordinator:route(
                   State#state.node,
                   {put, #data{key = Key, flags = Flags, value = Value}, State#state.quorum}
            ) of
                ok ->
                    gen_tcp:send(Socket, <<"STORED\r\n">>),
                    artifact_stat:incr_cmd_set(),
                    artifact_stat:add_bytes_write(#data{value=Value}),
                    {noreply, State};
                _Other ->
                    send_error_and_close("Failed to write.", State)
            end;
        _Other ->
            {noreply, State}
    end.

send_error_and_close(Message, State) ->
    ?warning(io_lib:format("send_error_and_close/2: ~p", [Message])),
    {close, ["Danger Will Robinson! Server Error:  ", Message, "\r\n"], State}.

cas_unique(CasUniqueInBinary) ->
    <<HashedValue:64/integer>> = CasUniqueInBinary,
    HashedValue.
