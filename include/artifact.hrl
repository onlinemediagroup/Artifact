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

-define(debug(_Data), ok).

-define(TIMEOUT, 5000).
