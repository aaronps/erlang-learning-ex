%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 12:54 PM
%%%-------------------------------------------------------------------
-module(curling_scoreboard_hw).
-author("krom").

%% API
-export([set_teams/2, next_round/0, add_point/1, reset_board/0]).

set_teams(TeamA, TeamB) ->
    io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

next_round() ->
    io:format("Scoreboard: round over.~n").

add_point(Team) ->
    io:format("Scoreboard: increase score for team ~s by 1~n", [Team]).

reset_board() ->
    io:format("Scoreboard: reset teams and score to 0~n").
