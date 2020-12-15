-module(server).

-export([start/0, start_link/0, stop/1, add_event/3, subscribe/1]).
-export([init/1]).

-type name() :: string().

-type monitor_ref() :: any().

-record(event, {
    owner :: pid(),
    name :: name(),
    pid :: pid()
}).

-record(state, {
    clients = #{} :: #{monitor_ref() := pid()},
    events = #{} :: #{name() := #event{}}
}).

-spec start() -> pid().
start() ->
    spawn(?MODULE, init, [#state{}]).

-spec start_link() -> pid().
start_link() ->
    spawn_link(?MODULE, init, [#state{}]).

-spec stop(Pid :: pid()) -> shutdown.
stop(Pid) ->
    Pid ! shutdown.

-spec subscribe(Pid :: pid()) -> {ok, monitor_ref()} | {error, atom()}.
subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    Pid ! {self(), Ref, {subscribe}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 -> {error, timeout}
    end.

-spec add_event(Pid :: pid(), Name :: string(), Delay :: integer()) -> ok | {error, timeout}.
add_event(Pid, Name, Delay) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, {add, Name, Delay}},
    receive
        {Ref, ok} ->
            ok
    after 5000 -> {error, timeout}
    end.

-spec init(State :: #state{}) -> ok.
init(State = #state{}) ->
    loop(State).

-spec loop(State :: #state{}) -> ok.
loop(State = #state{}) ->
    receive
        {Client, MsgRef, {subscribe}} ->
            Ref = erlang:monitor(process, Client),
            Clients = maps:put(Ref, Client, State#state.clients),
            Client ! {MsgRef, ok},
            loop(State#state{clients = Clients});
        {Client, MsgRef, {add, Name, Delay}} ->
            EventPid = event:start_link(Name, Delay),
            Event = #event{owner = Client, name = Name, pid = EventPid},
            Events = maps:put(Name, Event, State#state.events),
            Client ! {MsgRef, ok},
            loop(State#state{events = Events});
        {Client, MsgRef, {cancel, Name}} ->
            case maps:find(Name, State#state.events) of
                {ok, #event{pid = EventPid}} ->
                    ok = event:cancel(EventPid),
                    Events = map:put(Name, State#state.events),
                    Client ! {MsgRef, ok},
                    loop(State#state{events = Events});
                error ->
                    Client ! {MsgRef, {error, not_found}},
                    loop(State)
            end;
        {done, Name} ->
            #event{owner = OwnerPid} = maps:get(Name, State#state.events),
            OwnerPid ! {done, Name},
            Events = maps:remove(Name, State#state.events),
            loop(State#state{events = Events});
        shutdown ->
            ok;
        {'DOWN', Ref, process, _Pid, _Reason} ->
            Clients = maps:remove(Ref, State#state.clients),
            loop(State#state{clients = Clients})
    end.
