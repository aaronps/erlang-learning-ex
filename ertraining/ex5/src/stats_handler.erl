%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 8:30 PM
%%%-------------------------------------------------------------------
-module(stats_handler).
-author("krom").

%% API
-export([init/1, terminate/1, handle_event/2]).

init(_) -> [].

terminate(HandlerData) -> HandlerData.

handle_event({Type, _, Description}, HandlerData) ->
  insert_or_update(HandlerData, {Type, Description});

handle_event(_Event, HandlerData) -> HandlerData.

insert_or_update(DB, Key) ->
  case lists:keyfind(Key, 1, DB) of
    false -> io:format("New tuple: ~w~n",[Key]), lists:keystore(Key, 1 , DB, {Key, 1});
    {Key, OldValue} -> io:format("Update tuple: ~w:~w~n", [Key, OldValue+1]), lists:keystore(Key, 1 , DB, {Key, OldValue + 1})
  end.