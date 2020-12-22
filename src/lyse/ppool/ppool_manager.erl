-module(ppool_manager).

-behaviour(supervisor).

-export([
    start_link/0,
    stop/0,
    start_pool/3,
    stop_pool/1
]).

-export([init/1]).

-define(Pool, ppool).
-define(PoolSup, ppool_sup).

% external API

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?Pool}, ?MODULE, []).

-spec stop() -> true.
stop() ->
    case whereis(?Pool) of
        Id when is_pid(Id) ->
            exit(Id, kill);
        _ ->
            true
    end.

-spec start_pool(ppool:child_id(), pos_integer(), types:mfargs()) -> supervisor:startchild_ret().
start_pool(ChildId, MaxChildren, ChildMFA) ->
    ChildSpec = #{
        id => ChildId,
        start => {?PoolSup, start_link, [ChildId, MaxChildren, ChildMFA]},
        restart => permanent,
        shutdown => 10500,
        type => supervisor,
        modules => [?PoolSup]
    },
    supervisor:start_child(?Pool, ChildSpec).

stop_pool(ChildId) ->
    supervisor:terminate_child(?Pool, ChildId),
    supervisor:delete_child(?Pool, ChildId).

% supervisor callbacks

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 6,
        period => 3600
    },
    {ok, {SupFlags, []}}.
