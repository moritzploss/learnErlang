-module(curl_scoreboard_handler).

-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

init([]) ->
    {ok, []}.

handle_event({set_teams, TeamA, TeamB}, State) ->
    ok = curl_scoreboard:set_teams(TeamA, TeamB),
    {ok, State};
handle_event({add_points, Team, Points}, State) ->
    [curl_scoreboard:add_point(Team) || _ <- lists:seq(1, Points)],
    {ok, State};
handle_event(next_round, State) ->
    curl_scoreboard:next_round(),
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
