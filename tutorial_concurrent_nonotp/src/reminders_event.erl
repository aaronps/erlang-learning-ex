%% @author krom
%% @doc @todo Add description to reminders_event.


-module(reminders_event).
-vsn("0.1.2").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, start_link/2, init/3, cancel/1]).

-record(state, {server, name = "", to_go = 0}).

start(EventName, DateTime) ->
    spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
    spawn_link(?MODULE, init, [self(), EventName, DateTime]).

cancel(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

time_to_go(TimeOut = {{_, _, _}, {_, _, _}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
    Secs = if
               ToGo > 0 -> ToGo;
               ToGo =< 0 -> 0
           end,
    Secs.

init(Server, EventName, DateTime) ->
    loop(#state{server = Server, name = EventName, to_go = time_to_go(DateTime) * 1000}).

loop(State = #state{server = Server}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after State#state.to_go ->
        Server ! {done, State#state.name}
    end.
