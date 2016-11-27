%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2016 3:48 PM
%%%-------------------------------------------------------------------
-module(reminders_supervisor).
-author("krom").

%% API
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod,Args) ->
    spawn(?MODULE, init, [{Mod,Args}]).

start_link(Mod,Args) ->
    spawn_link(?MODULE, init, [{Mod,Args}]).

init({Mod,Args}) ->
    process_flag(trap_exit, true),
    loop({Mod,start_link,Args}).

loop({Mod, Fun, Args}) ->
    Pid = apply(Mod, Fun, Args),
    receive
        {'EXIT', _from, shutdown } ->
            exit(shutdown);
        {'EXIT', Pid, Reason } ->
            io:format("Process ~p abnormal exit due to ~p~n", [Pid, Reason]),
            loop({Mod,Fun,Args})
    end.

