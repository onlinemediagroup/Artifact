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

-module(artifact_tcp_server).

-export([behaviour_info/1]).
-export([start_link/1, start_link/2, start_link/3, start_link/4]).
-export([stop/0, stop/1]).
-export([info/1, info/2]).

-include("artifact.hrl").

% Behaviour Callbacks
behaviour_info(callbacks) -> [{init, 1}, {handle_call, 3}];
behaviour_info(_Other)    -> undefined.

% External APIs
start_link(Mod)       -> start_link(Mod, []).
start_link(Mod, Args) -> start_link(Mod, Args, #tcp_server_option{}).
start_link(Mod, Args, Option) ->
    start_link({local, ?MODULE}, Mod, Args, Option).
start_link(Name, Mod, Args, Option) ->
    artifact_tcp_server_sup:start_link(Name, Mod, Args, Option).

stop() -> stop(?MODULE).
stop(Name) ->
    artifact_tcp_server_sup:stop(Name).

info(Key) -> info(?MODULE, Key).
info(Name, Key) ->
    artifact_tcp_server_monitor:info(
        artifact_tcp_server_sup:build_monitor_name(Name), Key
    ).

