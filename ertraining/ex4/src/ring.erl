%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 11:10 AM
%%%-------------------------------------------------------------------
-module(ring).
-author("krom").

%% API
-export([start/3]).
-export([init/1]).

-compile(export_all).

start(NumProcess, NumMessages, Message) when NumProcess > 1 ->
  io:format("Going to create ring~n"),
  First = spawn_ring(1, NumProcess),
  io:format("Ring created, sending message~n"),
  First ! { message, Message, NumMessages},
  io:format("Message sent, quitting~n"),
  First ! quit,
  io:format("All finished~n").

spawn_ring(1, Max) ->
  Pid = spawn(?MODULE, init, [1]),
  Pid ! {link, spawn_ring(2, Max, Pid, Pid)},
  Pid.

spawn_ring(Max, Max, First, Last) ->
  Pid = spawn(?MODULE, init, [Max]),
  Pid ! { link, First},
  Pid;
spawn_ring(N, Max, First, Last) ->
  Pid = spawn(?MODULE, init, [N]),
  Pid ! {link, spawn_ring(N+1, Max, First, Pid)},
  Pid.

init(N) ->
  io:format("~w started ~w~n", [self(), N]),
  loop(N, no).

loop(N, Next) ->
  receive
    {link, Pid} ->
      io:format("~w linked to ~w~n",[self(), Pid]),
      loop(N, Pid);
    {message, Msg, 0} ->
      %io:format("~w last message: [~s]~n", [self(), Msg]),
      loop(N, Next);
    {message, Msg, Count} ->
      io:format("~w forward message: [~s]~n", [self(), Msg]),
      Next ! { message, Msg, Count -1},
      loop(N, Next);
    quit ->
      io:format("~w quitting~n", [self()]),
      Next ! quit
  end.



