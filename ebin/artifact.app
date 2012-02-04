{application, artifact, 
 [{description, "an artifact in a shattered visage lies"},
  {vsn, "0.4.0"},
  {modules, [
      artifact, 
      artifact_supervisor, 
      artifact_memcache, 
      artifact_rpc, 
      artifact_tcp_daemon,
      artifact_coordinator,
      artifact_membership,
      artifact_sync,
      artifact_connection,
      artifact_version,
      artifact_stat,
      artifact_storage,
      artifact_hashing,
      artifact_log, 
      artifact_config
  ]},
  {registered, [
      artifact_superviser, artifact_membership, artifact_sync, artifact_connection,
      artifact_version, artifact_stat, artifact_store, artifact_hashing, artifact_log,
      artifact_config
  ]},
  {applications, [kernel, stdlib]},
  {mod, {artifact, []}},
  {start_phases, []},
  {env, [
      {rpc_port, 11011},
      {rpc_max_processes, 30},
      {memcache_port, 11211},
      {memcache_max_processes, 10},
      {max_connections, 32},
      {n, 1}, {r, 1}, {w, 1},
      {number_of_buckets, 1024},
      {number_of_virtual_nodes, 128},
      {store, ets},
      {number_of_tables, 256}
  ]}
 ]}.