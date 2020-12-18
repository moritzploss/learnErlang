-module(musicians).

-behaviour(gen_server).

-export([
    start_link/2,
    stop/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(DELAY, 750).

-type skill() :: bad | ok | good | great | exceptional.

-type role() :: drums | guitar | keys | singer.

-record(state, {
    name = "" :: string(),
    role :: role(),
    skill = good :: skill()
}).

% external API

-spec start_link(Role :: role(), Skill :: skill()) -> {ok, pid()}.
start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, {Role, Skill}, []).

stop(Role) ->
    gen_server:call(Role, stop).

% callbacks

pick_name() ->
    "Mo".

pick_play_time() ->
    rand:seed(exs64),
    rand:uniform(3000).

-spec init({Role :: role(), Skill :: skill()}) -> {ok, #state{}, pos_integer()}.
init({Role, Skill}) ->
    process_flag(trap_exit, true),
    Name = pick_name(),
    io:format(
        "Musician ~s, playing the ~s entered the room~n",
        [Name, atom_to_list(Role)]
    ),
    {
        ok,
        #state{
            name = Name,
            role = Role,
            skill = Skill
        },
        pick_play_time()
    }.

handle_call(stop, _From, State = #state{}) ->
    {stop, normal, ok, State};
handle_call(_Message, _From, State = #state{}) ->
    {noreply, State, ?DELAY}.

handle_cast(_Args, State = #state{}) ->
    {noreply, State}.

handle_info(timeout, State = #state{skill = bad}) ->
    case rand:uniform(5) of
        1 ->
            {stop, bad_note, State};
        _ ->
            {noreply, State, ?DELAY}
    end;
handle_info(timeout, State = #state{}) ->
    io:format("~s produced sound!~n", [State#state.role]),
    {noreply, State, ?DELAY};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

terminate(bad_note, State = #state{}) ->
    io:format("~s sucks! kicked that member out of the band! (~s)~n", [
        State#state.name,
        State#state.role
    ]);
terminate(normal, State = #state{}) ->
    io:format("~s left the room (~s)~n", [State#state.name, State#state.role]);
terminate(shutdown, State = #state{}) ->
    io:format(
        "The manager is mad and fired the whole band! "
        "~s just got back to playing in the subway~n",
        [State#state.name]
    ).
