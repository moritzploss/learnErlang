-module(curl_scoreboard_handler_tests).

-include_lib("eunit/include/eunit.hrl").

curl_scoreboard_handler_test_() ->
    [
        {"Should handle set_teams event", handle_ev_set_teams()}
    ].

handle_ev_set_teams() ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, curl_scoreboard_handler, []),
    gen_event:notify(Pid, {set_teams, "Pirates", "Scotsmen"}),
    gen_event:delete_handler(Pid, curl_scoreboard_handler, turn_off).
