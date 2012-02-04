-record(data, {
    key, bucket, last_modified, vector_clocks, checksum, flags, value
}).

-record(tcp_server_option, {
    listen = [{active, false}, binary, {packet, line}, {reuseaddr, true}],
    port                    = 11211,
    max_processes           = 8,
    max_restarts            = 3,
    time                    = 60,
    shutdown                = 2000,
    accept_timeout          = infinity,
    accept_error_sleep_time = 3000,
    recv_length             = 0,
    recv_timeout            = infinity
}).

-define(error  (Data), artifact_log:log(error, self(), ?FILE, ?LINE, Data)).
-define(warning(Data), artifact_log:log(warning, self(), ?FILE, ?LINE, Data)).
-define(info   (Data), artifact_log:log(info, self(), ?FILE, ?LINE, Data)).

%-define(debug(Data), artifact_log:log(debug, self(), ?FILE, ?LINE, Data)).
-define(debug(_Data), ok).

-define(TIMEOUT, 5000).
-define(TIMER, 1000).
