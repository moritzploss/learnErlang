-module(ppool_serv).

-behaviour(gen_server).

-export([
    start/4,
    start_link/4,
    run/2,
    sync_queue/2,
    async_queue/2,
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

%% The friendly supervisor is started dynamically!
-define(SPEC(MFA),
    {worker_sup, {ppool_worker_sup, start_link, [MFA]}, temporary, 10000, supervisor, [
        ppool_worker_sup
    ]}
).

-record(state, {
    max_children = 0,
    sup,
    refs,
    queue = queue:new()
}).

start(ChildId, MaxChildren, Sup, MFA) when is_atom(ChildId), is_integer(MaxChildren) ->
    gen_server:start({local, ChildId}, ?MODULE, {MaxChildren, MFA, Sup}, []).

start_link(ChildId, MaxChildren, Sup, MFA) when is_atom(ChildId), is_integer(MaxChildren) ->
    gen_server:start_link({local, ChildId}, ?MODULE, {MaxChildren, MFA, Sup}, []).

run(ChildId, Args) ->
    gen_server:call(ChildId, {run, Args}).

sync_queue(ChildId, Args) ->
    gen_server:call(ChildId, {sync, Args}, infinity).

async_queue(ChildId, Args) ->
    gen_server:cast(ChildId, {async, Args}).

stop(ChildId) ->
    gen_server:call(ChildId, stop).

%% Gen server
init({MaxChildren, MFA, Sup}) ->
    %% We need to find the Pid of the worker supervisor from here,
    %% but alas, this would be calling the supervisor while it waits for us!
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{max_children = MaxChildren, refs = gb_sets:empty()}}.

handle_call({run, Args}, _From, S = #state{max_children = N, sup = Sup, refs = R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{max_children = N - 1, refs = gb_sets:add(Ref, R)}};
handle_call({run, _Args}, _From, S = #state{max_children = N}) when N =< 0 ->
    {reply, noalloc, S};
handle_call({sync, Args}, _From, S = #state{max_children = N, sup = Sup, refs = R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{max_children = N - 1, refs = gb_sets:add(Ref, R)}};
handle_call({sync, Args}, From, S = #state{queue = Q}) ->
    {noreply, S#state{queue = queue:in({From, Args}, Q)}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({async, Args}, S = #state{max_children = N, sup = Sup, refs = R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{max_children = N - 1, refs = gb_sets:add(Ref, R)}};
handle_cast({async, Args}, S = #state{max_children = N, queue = Q}) when N =< 0 ->
    {noreply, S#state{queue = queue:in(Args, Q)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs = Refs}) ->
    io:format("received down msg~n"),
    case gb_sets:is_element(Ref, Refs) of
        true ->
            handle_down_worker(Ref, S);
        %% Not our responsibility
        false ->
            {noreply, S}
    end;
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    {noreply, S#state{sup = Pid}};
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_down_worker(Ref, S = #state{max_children = L, sup = Sup, refs = Refs}) ->
    case queue:out(S#state.queue) of
        {{value, {From, Args}}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, S#state{refs = NewRefs, queue = Q}};
        {{value, Args}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            {noreply, S#state{refs = NewRefs, queue = Q}};
        {empty, _} ->
            {noreply, S#state{max_children = L + 1, refs = gb_sets:delete(Ref, Refs)}}
    end.
