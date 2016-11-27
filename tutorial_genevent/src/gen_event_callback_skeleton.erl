%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 1:27 PM
%%%-------------------------------------------------------------------
-module(gen_event_callback_skeleton).
-author("krom").
-behavior(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init([]) -> {ok, []}.

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
