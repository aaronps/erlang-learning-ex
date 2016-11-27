%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2016 10:47 PM
%%%-------------------------------------------------------------------
-module(erlcount_sup).
-author("krom").
-behavior(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    {ok, {{one_for_one, MaxRestart, MaxTime},
        [{dispatch,
            {erlcount_dispatch, start_link, []},
            transient,
            60000,
            worker,
            [erlcount_dispatch]}]}}.
