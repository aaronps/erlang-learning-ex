%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 2:22 PM
%%%-------------------------------------------------------------------
-module(curling_accumulator).
-author("krom").
-behavior(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {teams=orddict:new(), round=0}).

init([]) -> {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, State = #state{teams = Teams}) ->
    UpdatedTeams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, Teams)),
    {ok, State#state{teams=UpdatedTeams}};

handle_event({add_points, Team, N}, State = #state{teams = Teams}) ->
    UpdatedTeams = orddict:update_counter(Team, N, Teams),
    {ok, State#state{teams=UpdatedTeams}};

handle_event(next_round, State = #state{}) ->
    {ok, State#state{round = State#state.round + 1}};

handle_event(_, State) ->
    {ok, State}.

handle_call(game_data, State = #state{teams = Teams, round = Round}) ->
    {ok, {orddict:to_list(Teams), {round, Round}}, State};

handle_call(_, State) ->
    {ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.