%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2015 3:58 PM
%%%-------------------------------------------------------------------
-module(frequency).
-author("krom").

%% API
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() -> register(frequency, spawn(frequency, init, [])).

init() ->
  Frequencies = { get_frequencies(), [] },
  loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].

stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

call(Message) ->
  frequency ! { request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      case Frequencies of
        {_, []} -> reply(Pid, ok);
        _ -> reply(Pid, {error, some_frequency_used}), loop(Frequencies)
      end
  end.

reply(Pid, Reply) -> Pid ! {reply, Reply}.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};

allocate({[Freq | Free], Allocated}, Pid) ->
  CountFun = fun({_, UsedPid}, Acc) ->
    case UsedPid of
      Pid -> Acc + 1;
      _ -> Acc
    end
  end,
  PidUses = lists:foldl(CountFun, 0, Allocated),
  if
    PidUses < 3 -> { {Free, [{Freq,Pid} | Allocated]}, {ok, Freq}};
    true -> { {[Freq | Free], Allocated}, {error, too_many_allocated}}
  end.


deallocate({Free, Allocated}, Freq, Pid) ->
  case lists:keyfind(Freq, 1, Allocated) of
    {_, Pid} -> {{[Freq|Free], lists:keydelete(Freq, 1, Allocated)}, ok};
    {_, _} -> {{Free, Allocated}, {error, no_permissions}};
    false -> {{Free, Allocated}, {error, not_reserverd}}
  end.

