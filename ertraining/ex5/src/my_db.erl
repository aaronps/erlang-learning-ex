%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2015 10:43 AM
%%%-------------------------------------------------------------------
-module(my_db).
-author("krom").

%% API
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
%% for spawn
-export([init/1]).

start() -> register(my_db, spawn(?MODULE, init, [db:new()])).

stop() ->
  my_db ! { stop, self() },
  receive { reply, Reply } -> Reply end.

write(Key, Value) -> call({write, Key, Value}).
delete(Key)       -> call({delete, Key}).
read(Key)         -> call({read, Key}).
match(Value)      -> call({match, Value}).

%%
%% private funcs
%%

call(Msg) ->
  my_db ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

reply(To, Msg) -> To ! {reply, Msg}.

init(DB) -> loop(DB).

loop(DB) ->
  receive
    {request, From, Msg} ->
      {Reply, NewDB} = handle_msg(Msg, DB),
      reply(From, Reply),
      loop(NewDB);
    {stop, From} ->
      reply(From, terminate(DB))
  end.

terminate(DB) -> db:destroy(DB).

handle_msg({write, Key, Value}, DB) -> { ok, db:write(Key, Value, DB)};
handle_msg({delete, Key}, DB) -> {ok, db:delete(Key, DB)};
handle_msg({read, Key}, DB) -> {db:read(Key, DB), DB};
handle_msg({match, Value}, DB) -> {db:match(Value, DB), DB}.

