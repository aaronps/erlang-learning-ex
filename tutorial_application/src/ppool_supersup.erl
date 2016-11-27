%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 5:33 PM
%%%-------------------------------------------------------------------
-module(ppool_supersup).
-author("krom").
-behavior(supervisor).

%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).

-export([init/1]). %% this is the supervisor behavior

-define(SUPERSUP_REGISTERED_NAME, ppool).

%% This SUPER SUPERVISOR only cares about POOL SUPERVISOR

%% This creates a SUPER SUPERVISOR, registered with the name ppool,
%% @see init/1 for the specification
start_link() ->
    supervisor:start_link({local, ?SUPERSUP_REGISTERED_NAME}, ?MODULE, []).

%% This kills the supervisor badly, see the note.
%% NOTE: this is not the correct way to killing a supervisor, but it is explained in the
%% next chapter (this is for chapter 18, in chapter 19 it explains it).
stop() ->
    case whereis(?SUPERSUP_REGISTERED_NAME) of
        P when is_pid(P) ->
            exit(P, kill);
        _ ->
            ok
    end.

%% This returns the configuration for the SUPER SUPERVISOR, this configuration is:
%% It has not defined any child process
%% If (when) it has a child process and it dies, it will restart it (one_for_one)
%% If it restarts more than 6 times in one hour, it will DIE
init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime},[]}}.

%% Starts a pool, in fact it starts it's supervisor which would start the real pool.
%% The PoolName must be a atom because later the real pool server will register it as its name
%% The PoolName is used as ChildId in this supervisor, so later we can kill it.
%% The PoolName is also passed to the next supervisor, the one that creates the real pool, this
%% other supervisor will pass the PoolName to the final PoolServer which will register it as its
%% own name.
%% MFA IS the Module, Function, Arguments OF THE WORKERS!! (not it supervisor, not its server)
start_pool(PoolName, WorkerLimit, MFA) ->
    ChildSpec = {
        PoolName, %% PoolName becomes the childID in this SUPER SUPERVISOR, it must be unique
        {ppool_sup, start_link, [PoolName, WorkerLimit, MFA]},
        permanent, 10500, supervisor, [ppool_sup]},
    supervisor:start_child(?SUPERSUP_REGISTERED_NAME, ChildSpec).

%% Because we registered the PoolName as ChildId, we can now use it to kill it.
stop_pool(PoolName) ->
    supervisor:terminate_child(?SUPERSUP_REGISTERED_NAME, PoolName),
    supervisor:delete_child(?SUPERSUP_REGISTERED_NAME, PoolName).

