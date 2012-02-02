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
-module(artifact_supervisor).
-behaviour(supervisor).
    
-export([start_link/1]).
-export([init/1]).
    
-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init(Args) ->
    Config = {
        artifact_config,
        {artifact_config, start_link, [Args]},
        permanent, 1000, worker,
        [artifact_config]
    },
    Log = {
        artifact_log,
        {artifact_log, start_link, []},
        permanent, 1000, worker,
        [artifact_log]
    },
    Hash = {
        artifact_hash,
        {artifact_hash, start_link, []},
        permanent, 1000, worker,
        [artifact_hash]
    },
    Store = {
        artifact_store,
        {artifact_store, start_link, []},
        permanent, 1000, worker,
        [artifact_store]
    },
    Stat = {
        artifact_stat,
        {artifact_stat, start_link, []},
        permanent, 1000, worker,
        [artifact_stat]
    },
    Version = {
        artifact_version,
        {artifact_version, start_link, []},
        permanent, 1000, worker,
        [artifact_version]
    },
    Connection = {
        artifact_connection,
        {artifact_connection, start_link, []},
        permanent, 1000, worker,
        [artifact_connection]
    },
    Sync = {
        artifact_sync,
        {artifact_sync, start_link, []},
        permanent, 1000, worker,
        [artifact_sync]
    },
    Membership = {
        artifact_membership,
        {artifact_membership, start_link, []},
        permanent, 1000, worker,
        [artifact_membership]
    },
    Rpc = {
        artifact_rpc,
        {artifact_rpc, start_link, []},
        permanent, 1000, worker,
        [artifact_rpc]
    },
    Memcache = {
        artifact_memcache,
        {artifact_memcache, start_link, []},
        permanent, 1000, worker,
        [artifact_memcache]
    },
    {ok, {{one_for_one, 3, 10}, [
        Config, Log, Hash, Store, Stat, Version, Connection, Sync, Membership,
        Rpc, Memcache
    ]}}.
