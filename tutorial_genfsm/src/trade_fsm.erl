%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2016 9:35 PM
%%%-------------------------------------------------------------------
-module(trade_fsm).
-author("krom").

-behavior(gen_fsm).

%% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2,
         retract_offer/2, ready/1, cancel/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state callbacks
-export([
        idle/2, %% usado en FSM -> FSM
        idle/3,  %% usado en Cliente -> FSM
        idle_wait/2,
        idle_wait/3,
        negotiate/2,
        negotiate/3,
        wait/2,
        ready/2,
        ready/3
        ]).

%%% Public API: These functions all called by each own client to its own FSM

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%% trade and accept_trade
%% ----------------------
%% trade is used by one client to ask someone else if wants to trade
%% accept_trade is used by someone else to accept trade when it has been asked
%% These two must be synchronous, otherwise strange things could happen with the fsm
%% ---> in its "book" description, maybe another design would avoid these problems...

%% asks another fsm for trade, called by user, will crash if timeout times out
trade(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% accepts the previous trade request (negotiate), called by user,
%% will crash if not back in 5 seconds, due to default timeout
accept_trade(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% add item to the trade, called by user
make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% Cancel trade offer, called by user
retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Notify that you are ready, called by user, will wait for ever or crash,
%% if it returns, the return value is:
%%      when the other party is ready -> ok
%%      when the other party change their offers -> offer_changed
%% THE USER MUST CHECK THE RETURN VALUE AND ACT IN CONSEQUENCE
ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% Cancel transaction, called by user
cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%%% Intra FSM functions, these are called between the two different FSM, doesn't need to be exported

%% Ask the other fsm for trade session
%% this is the same as trade/2 but without the timeout
ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%%
%% This is the same as accept_trade/1 but asynchronous
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).

%% State record and utility functions

-record(state, {name="",
                other,          %% other is THE OTHER FSM pid
                ownitems = [],
                otheritems = [],
                monitor,        %% monitor is THE OTHER FSM MONITOR
                from            %% from is THIS CLIENT pid
                }).

%%% used to show a notice to the user on the console.
notice(#state{name=Name}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [Name | Args]).

unexpected(Msg, State) ->
    io:format("UNEXPECTED: ~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

add(Item, Items) -> [Item | Items].

remove(Item, Items) -> Items -- [Item].

priority(pid1, pid2) when pid1 > pid2 -> true;
priority(pid1, pid2) when pid1 < pid2 -> false.

%%% gen_fsm callbacks

init(Name) -> {ok, idle, #state{name=Name}}.

%% the function name is the state, depending on the parameters it gets the events, sync/async, etc

%%% DOING NOTHING

%% I (the fsm), while doing nothing was asked for negotiation by
%% another fsm -> tell the user and go to wait mode.
idle({ask_negotiate, OtherPid}, State = #state{}) ->
    Ref = monitor(process,OtherPid),
    notice(State, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, State#state{other = OtherPid, monitor = Ref}};

idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

%% I (the fsm), while doing nothing, the user asked me to ask someone else fsm
%% for negotiation -> send message and go wait mode
idle({negotiate, OtherPid}, From, State = #state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(State, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, State#state{other=OtherPid, monitor=Ref, from=From}};

idle(Event, _From, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

%%% WAITING for initial negotiation confirmation

%% I (the fsm) while waiting, was asked for negotiation by someone else's fsm
%% so -> tell the user and enter negotiation mode
idle_wait({ask_negotiate, OtherPid}, State = #state{other=OtherPid}) ->
    gen_fsm:reply(State#state.from, ok),
    notice(State, "Starting negotiation(a)", []),
    {next_state, negotiate, State};

%% I (the fsm) while waiting, was told by someone else's fsm that their user accepted
%% the negotiation, tell my user and go in negotiation mode
%% (this comes from accept_negotiate/2, called by someone else's fsm)
idle_wait({accept_negotiate, OtherPid}, State = #state{other=OtherPid}) ->
    gen_fsm:reply(State#state.from, ok),
    notice(State, "Starting negotiation(b)", []),
    {next_state, negotiate, State};

idle_wait(Event,Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

%% (this comes from accept_trade/1, called by user)
%% I (the fsm), while waiting, was told by the user to accept the trade
%% so -> send message to the other's fsm, tell the user and enter negotiation mode.
idle_wait(accept_negotiate, _From, State=#state{other=OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(State, "Accepting negotiation", []),
    {repply, ok, negotiate, State};

idle_wait(Event, _From, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

%%% NEGOTIATING

%% (comes from make_offer/2, called by user)
%% the user told me to make an offer, so I do it
negotiate({make_offer, Item}, State = #state{ownitems = OwnItems}) ->
    do_offer(State#state.other, Item),
    notice(State, "offering ~p to the other", [Item]),
    {next_state, negotiate, State#state{ownitems = add(Item, OwnItems)}};

%% (comes from retract_offer, called by user)
%% the user told me to retract an offer, so do it.
negotiate({retract_offer, Item}, State = #state{ownitems = OwnItems}) ->
    undo_offer(State#state.other, Item),
    notice(State, "cancelling offer of ~p to the other", [Item]),
    {next_state, negotiate, State=#state{ownitems = remove(Item, OwnItems)}};

%% (comes from do_offer/2, called by someone else's fsm)
%% someone else's fsm told me his user offers an item -> tell my user and update data
negotiate({do_offer, Item}, State = #state{otheritems = OtherItems}) ->
    notice(State, "The other is offering ~p~n", [Item]),
    {next_state, negotiate, State#state{otheritems = add(Item, OtherItems)}};

%% (comes from undo_offer/2, called by someone else's fsm)
%% someone else's fsm told me his user retracted from offering an item
%% -> tell my user and update data
negotiate({undo_offer, Item}, State = #state{otheritems = OtherItems}) ->
    notice(State, "The other RETRACTED from offering ~p~n", [Item]),
    {next_state, negotiate, State#state{otheritems = remove(Item, OtherItems)}};

%% (comes from are_you_ready/1, called by someone else's fsm)
%% someone else's fsm told me his user is ready
%% -> tell my user and reply we are not ready.
negotiate(are_you_ready, State = #state{other = OtherPid}) ->
    notice(State,
                "The other user is ready to trade, operation is:~n"
                "You get: ~p~nHe gets: ~p~n",
                [State#state.otheritems, State#state.ownitems]),
    not_yet(OtherPid),
    {next_state, negotiate, State};

negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

%% (comes from ready/1 which is waiting for ever, called by user)
%% The user told me he is ready
%% -> Tell it to someone else's fsm, save the From field for later notification and go wait mode
negotiate(ready, From, State = #state{other = OtherPid}) ->
    are_you_ready(OtherPid),
    notice(State, "Asking if the Other is waiting...~n", []),
    {next_state, wait, State#state{from = From}};

negotiate(Event, _From, State) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, State}.


%%% WAIT mode, I am ready, waiting for someone else to be

%% (comes from do_offer/2 , called by someone else's fsm)
%% the other added something to the trade
%% -> Tell my user, which is waiting a reply, with 'offer_changed', and update data
%% THEN go back to negotiating
wait({do_offer, Item}, State = #state{otheritems = OtherItems}) ->
    gen_fsm:reply(State#state.from, offer_changed),
    notice(State, "Now the other side is offering ~p", [Item]),
    {next_state, negotiate, State#state{otheritems = add(Item, OtherItems)}};

%% (comes from undo_offer/2 , called by someone else's fsm)
%% the other removed something from the trade
%% -> Tell my user, which is waiting a reply, with 'offer_changed', and update data
%% THEN go back to negotiating
wait({undo_offer, Item}, State = #state{otheritems = OtherItems}) ->
    gen_fsm:reply(State#state.from, offer_changed),
    notice(State, "Now the other side is retracting ~p", [Item]),
    {next_state, negotiate, State#state{otheritems = remove(Item, OtherItems)}};

%% (comes from not_yet/1, called by someone else's fsm)
%% The other isn't ready, but I am
%% -> tell the user, continue waiting
wait(not_yet, State = #state{}) ->
    notice(State, "The other is not ready yet", []),
    {next_state, wait, State};

%% (comes from are_you_ready/1, called by someone else's fsm)
%% someone else's fsm told me his user is ready, which I am
%% -> tell someone else's fsm that I am ready, tell my user that we are BOTH ready,
%% and continue waiting.
wait(are_you_ready, State = #state{}) ->
    am_ready(State#state.other), %% this sends the other's fsm state to the next 'ready!'
    notice(State, "The other asked for ready, I am ready, We are BOTH ready.", []),
    {next_state, wait, State};

%% (comes from am_ready/1, which is called by someone else's fsm)
%% The other was already ready when I asked
%% -> Tell the other I am ready too, ack to the transaction,
%% send repply to user (because he is waiting), then go to ready state.
%% NOTE: There is an extra ready! sent
%% NOTE2: This 'are_you_ready' and then send 'ready!' (2 messages) are needed because
%% the protocol specification says so.
wait('ready!', State = #state{}) ->
    am_ready(State#state.other), %% This will send an extra 'ready!'
    ack_trans(State#state.other), %% will send ack
    gen_fsm:reply(State#state.from, ok),
    notice(State, "The other is ready ready, going to ready state!!!", []),
    {next_state, ready, State};

wait(Event, Data) ->
    unexpected(Event, wait),
    {next_state, wait, Data}.

%% (comes from ack_trans/1 which was sent from wait('ready!'), so was called by someone else's fsm)
%% This message signals the beginning of the commit operation, we decide that one fsm will be master
%% (master) -> sync ask the other fsm for readiness to commit
%%          -> tells the other to commit
%%          -> commit itself
%% The request are synchronous and awaits specific values, it might crash any moment which will
%% finish the fsm.
%% (not master) -> continues in ready state, will receive some other sync requests.
ready(ack, State = #state{}) ->
    case priority(self(), State#state.other) of
        true ->
            try
                notice(State, "asking for commit", []),
                ready_commit = ask_commit(State#state.other),
                notice(State, "The other said it is ready to commit, ordering it", []),
                ok = do_commit(State#state.other),
                notice(State, "commiting as master...", []),
                commit(State),
                {stop, normal, State}
            catch
                Class : Reason ->
                    notice(State, "Commit failed: ~p:~p", [Class, Reason]),
                    {stop, {Class,Reason}, State}
            end;
        false ->
            {next_state, ready, State}
    end;

ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

%% (comes from ask_commit/1, which was called on the ready(ack) of the master)
%% Just reply that we are ready, this is only to pre-verify that we are all in good condition
ready(ask_commit, _From, State = #state{}) ->
    notice(State, "repply to ask_commit", []),
    {reply, ready_commit, ready, State};

%% (comes from do_commit/1, which was called on the ready(ack) of the master)
%% This is an order to do the real commit.
ready(do_commit, _From, State = #state{}) ->
    notice(State, "comitting as slave...", []),
    commit(State),
    {stop, normal, ok, State};

ready(Event, _From, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

commit(State = #state{}) ->
    io:format("Commit of transaction for ~s.~nYou give: ~p~nYou get: ~p~n",
              [State#state.name, State#state.ownitems, State#state.otheritems]).

%%% Other things

%% (comes from notify_cancel/1, called by someone else's fsm)
%% note that this one with 3 parameters is asynchronous
handle_event(cancel, _StateName, State = #state{}) ->
    notice(State, "Received a cancel event", []),
    {stop, other_cancelled, State};

handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% (comes from cancel/1, called by the user)
%% The user wants to cancel
%% -> Tell the other fsm, and stop with an ok message sent back to the waiting user.
handle_sync_event(cancel, _From, _StateName, State = #state{}) ->
    notify_cancel(State#state.other),
    notice(State, "User cancelled trade, telling the other", []),
    {stop, cancelled, ok, State};

%% NOTE: because we don't reply, the caller might crash due to timeout...
%% how about the one with infinity?
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.


handle_info({'DOWN', Ref, process, Pid, Reason}, _StateName, State = #state{other = Pid, monitor = Ref}) ->
    notice(State, "The other died", []),
    {stop, {other_down, Reason}, State};

handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

terminate(normal, ready, State = #state{}) ->
    notice(State, "FSM terminated.", []);
terminate(Reason, _StateName, State) ->
    notice(State, "FSM terminated by some other reason: ~p~n", [Reason]).


