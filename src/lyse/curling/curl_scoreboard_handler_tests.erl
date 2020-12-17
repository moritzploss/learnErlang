-module(curl_scoreboard_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertOK(X), ?_assertEqual(ok, X)).

beforeAll() ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, curl_scoreboard_handler, []),
    Pid.

afterAll(Pid) ->
    gen_event:delete_handler(Pid, curl_scoreboard_handler, []).

curl_scoreboard_handler_test_() ->
    {
        setup,
        fun beforeAll/0,
        fun afterAll/1,
        fun(Pid) ->
            [
                {"Should handle set_teams event", handle_ev_set_teams(Pid)}
            ]
        end
    }.

handle_ev_set_teams(Pid) ->
    Result = gen_event:notify(Pid, {set_teams, "Pirates", "Scotsmen"}),
    ?_assertOK(Result).
