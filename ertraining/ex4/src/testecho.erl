%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 10:56 AM
%%%-------------------------------------------------------------------
-module(testecho).
-author("krom").

%% API
-export([run/0]).

run() ->
  ok = echo:start(),
  ok = echo:print("PRINT"),
  ok = echo:stop().