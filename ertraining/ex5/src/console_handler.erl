%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 8:30 PM
%%%-------------------------------------------------------------------
-module(console_handler).
-author("krom").

%% API
-export([init/1, terminate/1, handle_event/2]).

init(HandlerData) -> HandlerData.

terminate(HandlerData) -> HandlerData.

handle_event({raise_alarm, Id, Alarm}, HandlerData) ->
  io:format("[~s] Begin ~w: ~s~n", [HandlerData, Id, Alarm]),
  HandlerData;

handle_event({clear_alarm, Id, Alarm}, HandlerData) ->
  io:format("[~s] End ~w: ~s~n", [HandlerData, Id, Alarm]),
  HandlerData;

handle_event(_Event, HandlerData) -> HandlerData.
