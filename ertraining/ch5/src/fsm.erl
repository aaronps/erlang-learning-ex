%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 4:22 PM
%%%-------------------------------------------------------------------
-module(fsm).
-author("krom").

%% API
-export([]).

idle() ->
  receive
    {Number, incoming} ->
      start_ringing(),
      ringing(Number);
    off_hook ->
      start_tone(),
      dial()
  end.

ringing(Number) ->
  receive
    {Number, other_on_hook} ->
      stop_ringing(),
      idle();
    {Number, off_hook} ->
      stop_ringing(),
      connected(Number)
  end.

dial() ->
  receive
    {digit, D} -> dial();
    on_hook ->
      stop_tone(),
      idle()
  end.

connected(Number) ->
  receive
    {Number, other_on_hook} -> idle();
    on_hook -> idle()
  end.

start_ringing() -> ok.
start_tone() -> ok.
stop_ringing() -> ok.
stop_tone() -> ok.

