%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2016 7:48 AM
%%%-------------------------------------------------------------------
-module(ppool_serv).
-author("krom").
-behavior(gen_server).
%% API
%% Starting functions, for supervisors to use.
-export([start/4, start_link/4]).

%% Client API
-export([run/2, sync_queue/2, async_queue/2, stop/1]).

%% gen_server API
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).


%% The PoolName MUST BE AN ATOM because it is going to be registered locally as a name for this server
start(PoolName, WorkerLimit, PoolSupervisor, WorkerMFA) when is_atom(PoolName), is_integer(WorkerLimit) ->
    gen_server:start({local, PoolName}, ?MODULE, {WorkerLimit, WorkerMFA, PoolSupervisor}, []).

start_link(PoolName, WorkerLimit, PoolSupervisor, WorkerMFA) when is_atom(PoolName), is_integer(WorkerLimit) ->
    gen_server:start_link({local, PoolName}, ?MODULE, {WorkerLimit, WorkerMFA, PoolSupervisor}, []).

%%
%% Client API
%%

%% Runs a job, passing it some parameters.
%% PoolName is the atom identifying this ppool_serv which was registered at the beginning.
run(PoolName, WorkerArgs) ->
    gen_server:call(PoolName, {run, WorkerArgs}).

sync_queue(PoolName, WorkerArgs) ->
    gen_server:call(PoolName, {sync, WorkerArgs}, infinity).

async_queue(PoolName, WorkerArgs) ->
    gen_server:cast(PoolName, {async, WorkerArgs}).

stop(PoolName) ->
    gen_server:call(PoolName, stop).

%% Helpers

%% This creates the worker supervisor spec
-define(CreateWorkerSupervisorSpecHelper(WorkerMFA),
    {worker_sup, %% childid is same for all worker supervisor "within the pool servers" (each has only one)
        {ppool_worker_sup, start_link, [WorkerMFA]}, %% MFA is passed to the supervisor so it can create its childs
        %% permanent, %% this supervisor has to be restarted always. WRONG WRONG WRONG
        temporary, %% THAT WAS WRONG!!! revised on the page and on the code
        10000,
        supervisor,
        [ppool_worker_sup]}).

-record(pool_state, {worker_limit = 0,
                     worker_supervisor,
                     worker_refs, %% monitor of the workers, so we know when they die.
                     work_queue=queue:new() %% The worker's queue?
                    }).

init({WorkerLimit,WorkerMFA, PoolSupervisor}) ->
    %% we cannot execute the next line because gen_* behaviours waits until init/1 returns, and
    %% because we are calling supervisor:start_child on PoolSupervisor which is initializing right now, then it will
    %% deadlock and/or crash sometime later
    % {ok, WorkerSupervisorPid} = supervisor:start_child(PoolSupervisor, ?CreateWorkerSupervisorSpecHelper(WorkerMFA)),

    %% So we send a message to ourselves, handle it on handle_info/2 after init returns.
    self() ! {start_worker_supervisor, PoolSupervisor, WorkerMFA},

    {ok, #pool_state{worker_limit = WorkerLimit, worker_refs = gb_sets:empty()}}.

handle_info({'DOWN', Ref, process, _Pid, _Reason},
            PoolState = #pool_state{}) ->
%%    io:format("Something went down~n"),
    case gb_sets:is_element(Ref, PoolState#pool_state.worker_refs) of
        true ->
            handle_worker_down(Ref, PoolState);
        false ->
            {noreply, PoolState}
    end;

handle_info({start_worker_supervisor, PoolSupervisor, WorkerMFA}, PoolState = #pool_state{}) ->
    {ok, WorkerSupervisorPid} = supervisor:start_child(PoolSupervisor, ?CreateWorkerSupervisorSpecHelper(WorkerMFA)),
    % This was not in the paper book, and it is not explained why do we need to link with the WorkerSupervisor
    % but if we don't link, some extrange errors can appear on the tests.
    % Looks like that with the link, if the worker supervisor dies first, the ppool_serv will die too, doing nothing
    % so, the error of use after die which is what was happening won't happen, this looks like a dirty trick
    % and also, it has the problem the book says: spawn and link at different time might lose some 'DOWN' message.
    link(WorkerSupervisorPid),
    {noreply, PoolState#pool_state{worker_supervisor = WorkerSupervisorPid}};

handle_info(Msg,State) ->
    io:format("Unknown INFO message: ~p~n", [Msg]),
    {noreply, State}.

handle_call({run, WorkerArgs},
            _From,
            PoolState = #pool_state{worker_limit = WorkerLimit, worker_supervisor = WorkerSupervisor, worker_refs = WorkerRefs})
            when WorkerLimit > 0 ->
    {ok, NewWorkerPid} = supervisor:start_child(WorkerSupervisor, WorkerArgs),
    NewWorkerRef = erlang:monitor(process, NewWorkerPid),
    {reply, {ok, NewWorkerPid}, PoolState#pool_state{worker_limit = WorkerLimit-1, worker_refs=gb_sets:add(NewWorkerRef, WorkerRefs)}};

handle_call({run, _WorkerArgs},
            _From,
            PoolState = #pool_state{worker_limit = WorkerLimit})
            when WorkerLimit =< 0 ->
    {reply, noalloc, PoolState};

handle_call({sync, WorkerArgs},
            _From,
            PoolState = #pool_state{worker_limit = WorkerLimit, worker_supervisor = WorkerSupervisor, worker_refs = WorkerRefs})
            when WorkerLimit > 0 ->
    {ok, NewWorkerPid} = supervisor:start_child(WorkerSupervisor, WorkerArgs),
    NewWorkerRef = erlang:monitor(process, NewWorkerPid),
    {reply, {ok, NewWorkerPid}, PoolState#pool_state{worker_limit = WorkerLimit-1, worker_refs = gb_sets:add(NewWorkerRef, WorkerRefs)}};

handle_call({sync, WorkerArgs},
            From,
            PoolState = #pool_state{work_queue = WorkQueue}) ->
    {noreply, PoolState#pool_state{work_queue = queue:in({From, WorkerArgs}, WorkQueue)}};

handle_call(stop, _From, PoolState) ->
    {stop, normal, ok, PoolState};

handle_call(_Msg, _From, PoolState) ->
    {noreply, PoolState}.

handle_cast({async, WorkerArgs},
            PoolState = #pool_state{worker_limit = WorkerLimit, worker_supervisor = WorkerSupervisor, worker_refs = WorkerRefs})
            when WorkerLimit > 0 ->
    {ok, NewWorkerPid} = supervisor:start_child(WorkerSupervisor, WorkerArgs),
    NewWorkerRef = erlang:monitor(process, NewWorkerPid),
    {noreply, PoolState#pool_state{worker_limit = WorkerLimit-1, worker_refs = gb_sets:add(NewWorkerRef, WorkerRefs)}};

handle_cast({async, WorkerArgs},
            PoolState = #pool_state{worker_limit = WorkerLimit, work_queue = WorkQueue})
            when WorkerLimit =< 0 ->
    {noreply, PoolState#pool_state{work_queue = queue:in(WorkerArgs, WorkQueue)}};

handle_cast(_Msg, PoolState) ->
    {noreply, PoolState}.

code_change(_OldVsn, PoolState, _Extra) ->
    {ok, PoolState}.

terminate(_Reason, _PoolState) ->
    ok.

handle_worker_down(OldWorkerRef, PoolState) ->
    WorkerRefs = gb_sets:delete(OldWorkerRef, PoolState#pool_state.worker_refs),

    case queue:out(PoolState#pool_state.work_queue) of
        {{value, {From, WorkerArgs}}, UpdatedWorkQueue} ->
            {ok, NewWorkerPid} = supervisor:start_child(PoolState#pool_state.worker_supervisor, WorkerArgs),
            NewWorkerRef = erlang:monitor(process, NewWorkerPid),
            UpdatedWorkerRefs = gb_sets:insert(NewWorkerRef, WorkerRefs),
            gen_server:reply(From, {ok, NewWorkerPid}),
            {noreply, PoolState#pool_state{worker_refs = UpdatedWorkerRefs, work_queue = UpdatedWorkQueue}};

        {{value, WorkerArgs}, UpdatedWorkQueue} ->
            {ok, NewWorkerPid} = supervisor:start_child(PoolState#pool_state.worker_supervisor, WorkerArgs),
            NewWorkerRef = erlang:monitor(process, NewWorkerPid),
            UpdatedWorkerRefs = gb_sets:insert(NewWorkerRef, WorkerRefs),
            {noreply, PoolState#pool_state{worker_refs = UpdatedWorkerRefs, work_queue = UpdatedWorkQueue}};

        {empty, _UnmodifiedQueue} -> %% the queue was empty
            {noreply, PoolState#pool_state{worker_limit = PoolState#pool_state.worker_limit + 1, worker_refs = WorkerRefs}}
    end.