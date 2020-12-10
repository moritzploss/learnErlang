-module(event).

-export([start/2, start_link/2, cancel/1]).

-record(state, {server :: pid(), name :: string(), delay = [0] :: [integer()]}).

start(Name, Delay) ->
  spawn(?MODULE, init, [self(), Name, Delay]).

start_link(Name, Delay) ->
  spawn_link(?MODULE, init, [self(), Name, Delay]).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

init(Server, Name, Delay) ->
  loop(#state{server = Server, name = Name, delay = split_delay(Delay)}).

split_delay(Delay) ->
  Limit = 49 * 24 * 3600,
  [Delay rem Limit | lists:duplicate(Delay div Limit, Limit)].

loop(State = #state{server = Server, delay = [Current | Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
    after Current ->
            case Next of
              [] ->
                Server ! {done, State#state.name};
              _ ->
                loop(State#state{delay = Next})
            end
  end.

