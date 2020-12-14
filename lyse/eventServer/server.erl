-module(server).

-export([start/0, start_link/0, stop/1, add_event/3, subscribe/1]).
-export([init/1]).

-type event() :: {
  owner, pid(),
  name, string(), 
  pid, pid()
}.

-type name() :: string().

-type monitor_ref() :: any().

-record(event, {
  owner :: pid(),
  name :: name(),
  pid :: pid()
}).

-record(state, {
  clients = orddict:new() :: erlang:orddict(monitor_ref(), pid()),
  events = orddict:new() :: erlang:orddict(name(), event())
}).

start() ->
  spawn(?MODULE, init, [#state{}]).

start_link() ->
  spawn_link(?MODULE, init, [#state{}]).

stop(Pid) ->
  Pid ! shutdown.

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  Pid ! {self(), Ref, {subscribe}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Pid, Name, Delay) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, {add, Name, Delay}},
  receive
    {Ref, ok} ->
      ok
  after 5000 ->
    {error, timeout}
  end.

init(State = #state{}) ->
  loop(State).

loop(State = #state{}) ->
  receive
    {Client, MsgRef, {subscribe}} ->
      Ref = erlang:monitor(process, Client),
      Clients = orddict:store(Ref, Client, State#state.clients),
      Client ! {MsgRef, ok},
      loop(State#state{clients = Clients});

    {Client, MsgRef, {add, Name, Delay}} ->
      EventPid = event:start_link(Name, Delay),
      Event = #event{owner = Client, name = Name, pid = EventPid},
      Events = orddict:store(Name, Event, State#state.events),
      Client ! {MsgRef, ok},
      loop(State#state{events = Events});

    {Client, MsgRef, {cancel, Name}} ->
      case orddict:find(Name, State#state.events) of
        {ok, #event{pid = EventPid}} ->
          ok = event:cancel(EventPid),
          Events = orddict:erase(Name, State#state.events),
          Client ! { MsgRef, ok},
          loop(State#state{events = Events});
        error ->
          Client ! {MsgRef, {error, not_found}},
          loop(State)
      end;

    {done, Name} ->
      #event{owner = OwnerPid} = orddict:fetch(Name, State#state.events),
      OwnerPid ! {done, Name},
      Events = orddict:erase(Name, State#state.events),
      loop(State#state{events = Events});

    shutdown ->
        ok;

    {'DOWN', Ref, process, _Pid, _Reason} ->
      Clients = orddict:erase(Ref, State#state.clients),
      loop(State#state{clients = Clients})
  end.
