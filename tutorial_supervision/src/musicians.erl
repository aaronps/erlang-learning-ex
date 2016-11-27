%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 3:27 PM
%%%-------------------------------------------------------------------
-module(musicians).
-author("krom").
-behavior(gen_server).

%% API
-export([start_link/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state,{name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) -> gen_server:call(Role, stop).

init([Role, Skill]) ->
    process_flag(trap_exit, true), %% to know when the parent shuts down.
    random:seed(now()),
    TimeToPlay = random:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),
    io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
    {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

handle_call(stop, _From, State = #state{}) ->
    {stop, normal, ok, State};
handle_call(_Mesage, _From, State) ->
    {noreply, State, ?DELAY}.

handle_cast(_Message, State) ->
    {noreply, State, ?DELAY}.

%% 'timeout' is generated automatically due to the last parameter (?DELAY) returned in
%% all the functions.
handle_info(timeout, State = #state{name=Name, skill=good}) ->
    io:format("~s produced sound!~n",[Name]),
    {noreply, State, ?DELAY};

handle_info(timeout, State = #state{name=Name, skill=bad}) ->
    case random:uniform(5) of
        1 ->
            io:format("~s played a false note. Uh oh~n", [Name]),
            {stop, bad_note, State};

        _ ->
            io:format("~s produced sound!~n",[Name]),
            {noreply, State, ?DELAY}
    end;

handle_info(_Message, State) ->
    {noreply, State, ?DELAY}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, State) ->
    io:format("~s left the room (~s)~n", [State#state.name, State#state.role]);

terminate(bad_note, State) ->
    io:format("~s was kicked because sucks as a ~s!~n", [State#state.name, State#state.role]);

terminate(shutdown, State) ->
    io:format(  "The manager is mad and fired the whole band, "
                "~s just got back to play in the street!~n", [State#state.name]);

terminate(_Reason, State) ->
    io:format("~s was kicked out for unknown reasons~n", [State#state.name]).













pick_name() ->
    lists:nth(random:uniform(10), firstnames())
    ++ " " ++
    lists:nth(random:uniform(10), lastnames()).

firstnames() ->
    ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
     "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

lastnames() ->
    ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
     "Terese", "Tennelli", "Jamal" ,"Li", "Perlstein"].


