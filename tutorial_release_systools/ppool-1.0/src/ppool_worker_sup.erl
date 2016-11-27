%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 10:12 PM
%%%-------------------------------------------------------------------
-module(ppool_worker_sup).
-author("krom").
-behavior(supervisor).

%% API
-export([start_link/1, init/1]).

%%%% @@@@TODO unknown use of this.

%% MFA is the Module, Function, Args of the WORKER, it comes from the SUPER SUPERVISOR,
%% it was passed as a parameter to ppool_supersup/1 and handed down through some hops to here.
start_link(WorkerMFA = {_,_,_}) ->
    supervisor:start_link(?MODULE, WorkerMFA).

init({WorkerMod, WorkerFun, WorkerArgs}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
          [{ppool_worker,
            {WorkerMod, WorkerFun, WorkerArgs},
            temporary, 5000, worker, [WorkerMod]}
          ]}}.
