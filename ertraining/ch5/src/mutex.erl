%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 4:32 PM
%%%-------------------------------------------------------------------
-module(mutex).
-author("krom").

%% API
-export([start/0, stop/0, wait/0, signal/0, init/0]).


start() -> register(mutex, spawn(?MODULE, init, [])).

stop() -> mutex ! stop.

wait() ->
  mutex ! {wait, self()},
  receive ok -> ok end.

signal() -> mutex ! {signal, self()}, ok.

init() -> free().

free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop ->
      terminate()
  end.

busy(Pid) ->
  receive
    {signal, Pid} ->
      free()
  end.

%%
%% This terminate is used to flush the waiting pids of the queue, by killing them.
%% if there was not any message, then the timeout of 0 will run, finishing this process
%%
terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after
    0 -> ok
  end.
