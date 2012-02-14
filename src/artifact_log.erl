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

-module(artifact_log).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([log/5]).
-export([
    init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2,
    code_change/3
]).

-include("artifact.hrl").

-record(state, {fd}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).

init(_Args) ->
    case artifact_config:get(logfile) of
        undefined ->
            {ok, #state{}};
        File ->
            case file:open(File, [write, append]) of
                {ok, Fd} -> {ok, #state{fd=Fd}};
                Error    -> Error
            end
    end.

terminate(_Reason, State) ->
    case State#state.fd of
        undefined -> ok;
        Fd        -> file:close(Fd)
    end.

log(Type, Pid, File, Line, Data, State) ->
    {{Year,Month,Day}, {Hour,Minute,Second}} = erlang:localtime(),
    {_MegaSec, _Sec, Usec} = now(),
    Data2 =
        if
            is_list(Data) -> lists:flatten(Data);
            true          -> Data
        end,
    %% Hella format strings, sorry :( 
    Buf = io_lib:format(
        "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w [~s] (~p) ~s:~w: ~p\n",
        [Year, Month, Day, Hour, Minute, Second, Usec, Type, Pid, File, Line, Data2]
    ),
    case State#state.fd of
        undefined -> io:format(    "~s", [Buf]);
        Fd        -> io:format(Fd, "~s", [Buf])
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.
handle_cast({log, Type, Pid, File, Line, Data}, State) ->
    log(Type, Pid, File, Line, Data, State),
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
log(Type, Pid, File, Line, Data) ->
    gen_server:cast(?SERVER, {log, Type, Pid, File, Line, Data}).
