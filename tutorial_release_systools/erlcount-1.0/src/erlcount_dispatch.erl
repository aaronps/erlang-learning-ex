%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2016 7:30 AM
%%%-------------------------------------------------------------------
-module(erlcount_dispatch).
-author("krom").
-behavior(gen_fsm).

%% API
-export([start_link/0, complete/4]).

% gen_fsm
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4, code_change/4, terminate/3]).

% states
-export([dispatching/2, listening/2]).

-define(POOL, erlcount).

-record(data, {regex_counts = [], work_refs =[]}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

complete(FsmPid, Regex, MessageReference, Count) ->
    gen_fsm:send_all_state_event(FsmPid, {complete, Regex, MessageReference, Count}).

%% gen_fsm

init([]) ->
    {ok, ConfigRegex} = application:get_env(regex),
    {ok, ConfigDir} = application:get_env(directory),
    {ok, ConfigMaxFiles} = application:get_env(max_files),
    ppool:start_pool(?POOL, ConfigMaxFiles, {erlcount_counter, start_link, []}),
    case lists:all(fun valid_regex/1, ConfigRegex) of
        true ->
            self() ! {start, ConfigDir},
            {ok, dispatching, #data{regex_counts =[ {R,0} || R <- ConfigRegex]}};
        false ->
            {stop, invalid_regex}
    end.

handle_info({start, Directory}, FsmState, Data) ->
    gen_fsm:send_event(self(), erlcount_lib:find_erl(Directory)),
    {next_state, FsmState, Data}.

handle_event({complete, Regex, Ref, Count}, FsmState, Data = #data{regex_counts = RegexCounts, work_refs = Refs} ) ->
    {Regex, OldCount} = lists:keyfind(Regex, 1, RegexCounts),
    NewRegexCounts = lists:keyreplace(Regex, 1, RegexCounts, {Regex, OldCount + Count}),
    UpdatedData = Data#data{regex_counts = NewRegexCounts, work_refs = Refs -- [Ref]},
    case FsmState of
        dispatching ->
            {next_state, dispatching, UpdatedData};
        listening -> %% so it checks if finished, or continue like that.
            listening(done, UpdatedData)
    end.

handle_sync_event(Event, _From, FsmState, Data) ->
    io:format("Unexpected sync event: ~p~n", [Event]),
    {next_state, FsmState, Data}.

code_change(_OldVsn, FsmState, Data, _Extra) ->
    {ok, FsmState, Data}.

terminate(_Reason, _State, _Data) ->
	init:stop().

%% STATES

%% This might only come from handle_info(start) or from dispatching(continue) -> this same function!
%% It arrives after a send_event(self(), erlcount_lib:find_erl because, if there are erl files, it will return this.
dispatching({continue, File,  ContinuationFunction}, Data = #data{regex_counts = RegexCounts, work_refs = WorkRefs}) ->
    F = fun({Regex, _Count}, WorkRefList) ->
        NewWorkRef = make_ref(),
        ppool:async_queue_work(?POOL, [self(), NewWorkRef, File, Regex]),
        [NewWorkRef | WorkRefList]
    end,
    UpdatedWorkRefs = lists:foldl(F, WorkRefs, RegexCounts),
    gen_fsm:send_event(self(), ContinuationFunction()),
    {next_state, dispatching, Data#data{work_refs = UpdatedWorkRefs}};

%% when find_erl returns done, we call listening(done) which, if all work is done, just finished,
%% if, on the other hand, there is still work to do, it will change the state to listening.
%% this part is a little bit tricky, but it HAS TO BE LIKE THIS.
dispatching(done, Data) ->
    listening(done, Data).

listening(done, #data{regex_counts = RegexCounts, work_refs = []}) ->
    [io:format("Regex ~s has ~p results~n", [Regex, Count]) || {Regex, Count} <- RegexCounts],
    {stop, normal, done};

listening(done, Data) ->
    {next_state, listening, Data}.

%% private

valid_regex(Regex) ->
    try re:run("", Regex) of
        _ -> true
    catch
        error:badarg -> false
    end.
