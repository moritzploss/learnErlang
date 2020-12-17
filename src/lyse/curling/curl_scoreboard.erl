-module(curl_scoreboard).

-export([
    set_teams/2,
    next_round/0,
    add_point/1,
    reset_board/0
]).

-spec set_teams(string(), string()) -> ok.
set_teams(TeamA, TeamB) ->
    io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

-spec next_round() -> ok.
next_round() ->
    io:format("Scoreboard: round over~n").

-spec add_point(string()) -> ok.
add_point(Team) ->
    io:format("Scoreboard: increased score of team ~s by 1~n", [Team]).

-spec reset_board() -> ok.
reset_board() ->
    io:format("Scoreboard: reset teams and scores~n").
