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
-module(artifact_store).

-export([start_link/0, stop/0]).
-export([list/1, get/1, put/1, delete/1, info/1]).

-include("artifact.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    Store = artifact_config:get(store),
    Module = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Store)),
    apply(Module, start_link, [?SERVER]).

stop() ->
    gen_server:call(?SERVER, stop).
list(Bucket) ->
    gen_server:call(?SERVER, {list, Bucket}).
get(Data) ->
    gen_server:call(?SERVER, {get, Data}).
put(Data) ->
    gen_server:call(?SERVER, {put, Data}).
delete(Data) ->
    gen_server:call(?SERVER, {delete, Data}).
info(Name) ->
    gen_server:call(?SERVER, {info, Name}).
