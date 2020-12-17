-module(curl_accumulator).

-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2
]).

-record(state, {
    events = []
}).

init([]) ->
    {ok, #state{}}.

handle_event(Event, State) ->
    {ok, #state{events = [Event | State#state.events]}}.

handle_call(game_data, State = #state{events = Events}) ->
    {ok, lists:reverse(Events), State}.
