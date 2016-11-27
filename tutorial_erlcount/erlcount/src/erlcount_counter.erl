%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2016 8:51 AM
%%%-------------------------------------------------------------------
-module(erlcount_counter).
-author("krom").
-behavior(gen_server).

%% API
-export([start_link/4]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {dispatcher_pid, work_ref, file_name, regex}).

start_link(DispatcherPid, WorkRef, FileName, Regex) ->
    gen_server:start_link(?MODULE, [DispatcherPid, WorkRef, FileName, Regex], []).

% gen_server

init([DispatcherPid, WorkRef, FileName, Regex]) ->
    self() ! start,
    {ok, #state{dispatcher_pid = DispatcherPid, work_ref = WorkRef, file_name = FileName, regex = Regex}}.

handle_call(_Msg, _From, Data) ->
    {noreply, Data}.

handle_cast(_Msg, Data) ->
    {noreply, Data}.

handle_info(start, Data = #state{regex = Regex, work_ref = WorkRef}) ->
    io:format("Will scan ~s~n", [Data#state.file_name]),
    {ok, BinaryFileData} = file:read_file(Data#state.file_name),
    Count = erlcount_lib:regex_count(Regex, BinaryFileData),
    erlcount_dispatch:complete(Data#state.dispatcher_pid, Regex, WorkRef, Count),
    {stop, normal, Data}.

terminate(_Reason, _Data) ->
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.