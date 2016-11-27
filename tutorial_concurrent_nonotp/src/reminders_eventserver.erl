%% @author krom
%% @doc @todo Add description to reminders_eventserver.


-module(reminders_eventserver).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    start/0, start_link/0, terminate/0, upgrade/0,
    init/0, show_version/0,
    subscribe/1, add_event/3, cancel/1, listen/1,
    loop/1 %% needs to public loop!!!! otherwise code upgrade will fail
]).

-record(state, {events,        %% list of events
    clients}).    %% list of clients

-record(event, {name = "", description = "", pid, timeout = {{1970, 1, 1}, {0, 0, 0}}}).

-define(MYVERSION, "0.6").

start() ->
    register(?MODULE, Pid = spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

upgrade() ->
    ?MODULE ! code_change.

show_version() ->
    ?MODULE ! show_version_msg.


subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! { self(), Ref, {subscribe, Pid} },
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

add_event(Name, Description, Timeout) ->
    Ref = make_ref(),
    ?MODULE ! { self(), Ref, {add, Name, Description, Timeout}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! { self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        Message = {done, _Name, _Description} ->
            [ Message | listen(0) ]
    after Delay * 1000 ->
        []
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
    loop(#state{events = orddict:new(), clients = orddict:new()}).

loop(State = #state{}) ->
    io:format("########################### loop start " ?MYVERSION "~n"),
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, State#state.clients),
            Pid ! {MsgRef, ok},
            loop(State#state{clients = NewClients});

        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = reminders_event:start_link(Name, TimeOut),
                    NewEvents = orddict:store(Name,
                        #event{name = Name,
                            description = Description,
                            pid = EventPid,
                            timeout = TimeOut},
                        State#state.events),
                    Pid ! {MsgRef, ok},
                    loop(State#state{events = NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(State)
            end;

        {Pid, MsgRef, {cancel, Name}} ->
            NewEvents = case orddict:find(Name, State#state.events) of
                            {ok, Event} ->
                                reminders_event:cancel(Event#event.pid),
                                orddict:erase(Name, State#state.events);
                            error ->
                                State#state.events
                        end,
            Pid ! {MsgRef, ok},
            loop(State#state{events = NewEvents});

        {done, Name} ->
            case orddict:find(Name, State#state.events) of
                {ok, Event} ->
                    send_to_clients({done, Event#event.name, Event#event.description}, State#state.clients),
                    NewEvents = orddict:erase(Name, State#state.events),
                    loop(State#state{events = NewEvents});
                error ->
                    loop(State)
            end;

        shutdown ->
            exit(shutdown);

        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(State#state{clients = orddict:erase(Ref, State#state.clients)});

        code_change ->
            io:format("code_change in version " ?MYVERSION "~n"),
            ?MODULE:loop(State);

        show_version_msg ->
            io:format("Version is " ?MYVERSION "~n"),
            loop(State);

        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(State)
    end.

valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> false
    end;

valid_datetime(_) -> false.

valid_time({H, M, S}) -> valid_time(H, M, S).

valid_time(H, M, S) when H >= 0, H < 24,
    M >= 0, M < 60,
    S >= 0, S < 60 -> true;

valid_time(_, _, _) -> false.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).
