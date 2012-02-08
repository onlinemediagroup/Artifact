%   Artifact Database
% -  Zephyr Pellerin -
%     github - zv
% the only requirement
% of this software is 
% that you keep it real
% mixrank ------   2012

%% @copyright 2012 Zephyr Pellerin

%% @reference Amazon Dynamo

%% @author Zephyr Pellerin <zephyr@mixrank.com>

%% @doc A simple erlang distributed key value store.

-author('Zephyr Pellerin <zephyr.pellerin@gmail.com>').


-module(artifact).
-behaviour(application).

-export([start/2, stop/1]).
-export([start/0]).

config([], Acc) ->
    Acc;
config([Key|Rest], Acc) ->
    case application:get_env(artifact, Key) of
        undefined   -> config(Rest, Acc);
        {ok, Value} -> config(Rest, [{Key, Value}|Acc])
    end.

start(_Type, _Args) ->
    Args = config([
        logfile, hostname,
        rpc_port, rpc_max_processes,
        memcache_port, memcache_max_processes,
        max_connections,
        n, r, w,
        number_of_buckets, number_of_virtual_nodes,
        store, dets_dir, number_of_tables
    ], []),
    artifact_supervisor:start_link(Args).

stop(_State) ->
    ok.

start() ->
    application:start(artifact).
