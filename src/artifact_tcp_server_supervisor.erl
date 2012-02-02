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

-module(artifact_tcp_server_supervisor).
-behaviour(supervisor).

-export([start_link/4, stop/1]).
-export([init/1]).
-export([build_monitor_name/1]).

-include("artifact.hrl").

% External APIs
start_link(Name, Mod, Args, Option) ->
    supervisor:start_link(Name, ?MODULE, [Name, Mod, Args, Option]).

stop(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            exit(Pid, normal),
            ok;
        _ -> not_started
    end.

% Callbacks
init([Name, Mod, Args, Option]) ->
    case Mod:init(Args) of 
        {ok, State}    -> listen(State, Name, Mod, Option);
        {stop, Reason} -> Reason;
        Other          -> Other % 'ignore' is contained.
    end.

% Internal Functions
listen(State, Name, Mod, Option) ->
    case gen_tcp:listen(
        Option#tcp_server_option.port,
        Option#tcp_server_option.listen
    ) of
        {ok, ListenSocket} ->
            build_result(ListenSocket, State, Name, Mod, Option);
        {error, Reason} ->
            ?warning(io_lib:format("listen(~p) ~p", [Mod, {error, Reason}])),
            {stop, Reason}
    end.

build_result(ListenSocket, State, {Dest, Name}, Mod, Option) ->
    #tcp_server_option{
        max_restarts = MaxRestarts,
        time         = Time
    } = Option,
    MonitorName = build_monitor_name(Name),
    {ok, {
        {one_for_one, MaxRestarts, Time},
        [
            monitor_spec({Dest, MonitorName}) |
            acceptor_specs(
                ListenSocket, State, {Dest, Name}, MonitorName, Mod, Option
            )
        ]
    }}.

monitor_spec({Dest, MonitorName}) ->
    {
        MonitorName,
        {
            artifact_tcp_server_monitor,
            start_link,
            [{Dest, MonitorName}]
        },
        permanent,
        brutal_kill,
        worker,
        []
    }.

acceptor_specs(
  ListenSocket, State, {Dest, Name}, MonitorBaseName, Mod, Option
) ->
    #tcp_server_option{
        max_processes = MaxProcesses,
        shutdown      = Shutdown
    } = Option,
    MonitorName = case Dest of
      local   -> MonitorBaseName;
      _Global -> {Dest, MonitorBaseName}
    end,
    lists:map(
        fun (N) ->
            AcceptorName = build_acceptor_name(Name, N),
            {
                AcceptorName,
                {
                    artifact_tcp_server_acceptor,
                    start_link,
                    [
                        {Dest, AcceptorName},
                        ListenSocket,
                        State,
                        MonitorName,
                        Mod,
                        Option
                    ]
                },
                permanent,
                Shutdown,
                worker,
                []
            }
        end,
        lists:seq(1, MaxProcesses)
    ).

build_monitor_name(Prefix) ->
    list_to_atom(atom_to_list(Prefix) ++ "_monitor").

build_acceptor_name(Prefix, Number) ->
    list_to_atom(
        atom_to_list(Prefix) ++ "_acceptor_" ++ integer_to_list(Number)
    ).
 
