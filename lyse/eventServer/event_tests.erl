-module(event_tests).

-include_lib("eunit/include/eunit.hrl").

event_test_() ->
  [{"Event can be initialized",
    event_init()},
    {"'Done' is triggered after delay",
    event_done()},
    {"'Done' is triggered with correct event name",
    event_name()}].

event_init() ->
  Pid = event:start("Test", 100),
  ?_assert(erlang:is_process_alive(Pid)),
  ?_assert(exit(Pid, kill)).

event_done() ->
  Pid = event:start("Test", 0),
  receive
    {done, _Name} ->
      ?_assert(exit(Pid, kill))
    after 1000 ->
      error("Did not receive 'Done'")
  end.

event_name() ->
  Name = "Test",
  Pid = event:start(Name, 0),
  receive
    {done, Name} ->
      ?_assert(exit(Pid, kill))
    after 100 ->
      error("Did not receive 'Done' for event with name " ++ Name)
  end.

