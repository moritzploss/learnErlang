-module(server).

-export([start/0, start_link/0, stop/1]).
-export([init/1]).

-record(state, {
  clients = orddict:new() :: erlang:orddict(any(), pid()),
  events = orddict:new() :: erlang:orddict(string(), pid())
}).

start() ->
  spawn(?MODULE, init, [#state{}]).

start_link() ->
  spawn_link(?MODULE, init, [#state{}]).

stop(Pid) ->
  Pid ! shutdown.

init(State = #state{}) ->
  loop(State).

loop(State) ->
  receive
    {Client, MsgRef, {subscribe}} ->
      Ref = erlang:monitor(process, Client),
      Clients = orddict:store(Ref, Client, State#state.clients),
      Client ! {MsgRef, ok},
      loop(State#state{clients = Clients});

    {Client, MsgRef, {add, Name, Delay}} ->
      EventPid = event:start_link(Name, Delay),
      Events = orddict:store(Name, EventPid, State#state.events),
      Client ! {MsgRef, ok},
      loop(State#state{events = Events});

    {Client, MsgRef, {cancel, Name}} ->
      case orddict:find(Name, State#state.events) of
        {ok, EventPid} ->
          ok = event:cancel(EventPid),
          Events = orddict:erase(Name, State#state.events),
          Client ! { MsgRef, ok},
          loop(State#state{events = Events});
        error ->
          Client ! {MsgRef, {error, not_found}},
          loop(State)
      end;

    shutdown ->
        ok;

    {'DOWN', Ref, process, _Pid, _Reason} ->
      Clients = orddict:erase(Ref, State#state.clients),
      loop(State#state{clients = Clients})
  end.
