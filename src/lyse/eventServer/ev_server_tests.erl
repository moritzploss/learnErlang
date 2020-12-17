-module(ev_server_tests).

-include_lib("eunit/include/eunit.hrl").

event_test_() ->
    [{"Server can be started", server_start()}].

server_start() ->
    Pid = ev_server:start(),
    true = erlang:is_process_alive(Pid),
    ?_assert(exit(Pid, kill)).
