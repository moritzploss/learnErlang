-module(event_tests).

-include_lib("eunit/include/eunit.hrl").

event_test_() -> [
  ?_assert(event_init()),
  ?_assert(event_done())
].

event_init() ->
  event:start("Test", 0),
  true.

event_done() ->
  Name = "Test",
  event:start(Name, 0),
  receive
    {done, Name} ->
      true
    after 1000 ->
      false
  end.

