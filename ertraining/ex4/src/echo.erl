%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 10:47 AM
%%%-------------------------------------------------------------------
-module(echo).
-author("krom").

%% API
-export([start/0, print/1, stop/0]).
-export([init/0]).

start() -> register(echo, spawn(?MODULE, init, [])), ok.

print(Term) -> echo ! { print, Term}, ok.

stop() -> echo ! stop, ok.

init() -> loop().

loop() ->
  receive
    {print, Term} -> io:format("print: '~s'~n", [Term]), loop();
    stop -> ok
  end.
