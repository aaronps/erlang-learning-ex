%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2016 10:29 AM
%%%-------------------------------------------------------------------
-module(ppool).
-author("krom").
%%-behavior(application).

%% API
-export([start/2, stop/1, start_pool/3, stop_pool/1, run_work/2, async_queue_work/2, sync_queue_work/2]).

start(normal, _Args) ->
    ppool_supersup:start_link().

stop(_State) ->
    ok.
%%    ppool_supersup:stop().

start_pool(PoolName, WorkerLimit, {WorkerModule, WorkerFun, WorkerArgs}) ->
    ppool_supersup:start_pool(PoolName, WorkerLimit, {WorkerModule, WorkerFun, WorkerArgs}).

stop_pool(PoolName) ->
    ppool_supersup:stop_pool(PoolName).

run_work(PoolName, WorkArgs) ->
    ppool_serv:run(PoolName, WorkArgs).

async_queue_work(PoolName, WorkArgs) ->
    ppool_serv:async_queue(PoolName, WorkArgs).

sync_queue_work(PoolName, WorkArgs) ->
    ppool_serv:sync_queue(PoolName, WorkArgs).
