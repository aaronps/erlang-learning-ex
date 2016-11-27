%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 7:09 PM
%%%-------------------------------------------------------------------
-module(ppool_sup).
-author("krom").
-behavior(supervisor).
%% API
-export([start_link/3, init/1]).

%% Starts the POOL supervisor, the PoolName comes from the SUPER SUPERVISOR
%% The PoolName is passed to the init, continue there to know the details of this supervisor.
start_link(PoolName, WorkersLimit, MFA) ->
    supervisor:start_link(?MODULE, {PoolName, WorkersLimit, MFA}).

%% This supervisor is on_for_all, so if anything dies, all will be restarted.
%% @toknow Does this supervisor has only one child?
%% It seems this supervisor only has one child which is the real POOL SERVER, it is registered
%% as 'serv', all pools are the same internally.
%% The PoolName is passed to the POOL SERVER... right know don't know its use
%% THIS supervisor pid is also passed to the POOL SERVER, don't know the reason.
init({PoolName, WorkersLimit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_all, MaxRestart, MaxTime},
          [{serv, %% this is this childID within this supervisor
            {ppool_serv, start_link, [PoolName, WorkersLimit, self(), MFA]},
            permanent, 5000, worker, [ppool_serv]}
          ]}}.
