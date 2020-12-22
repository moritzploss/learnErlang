-module(ppool_sup).

-behaviour(supervisor).

-export([
    start_link/3,
    init/1
]).

-define(PoolServer, ppool_serv).

% public API

-spec start_link(ppool:child_id(), pos_integer(), types:mfargs()) -> supervisor:startlink_ret().
start_link(ChildId, MaxChildren, ChildMFA) ->
    supervisor:start_link(?MODULE, {ChildId, MaxChildren, ChildMFA}).

% supervisor callbacks

init({ChildId, MaxChildren, ChildMFA}) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 3600
    },
    ChildSpec = #{
        id => serv,
        start => {?PoolServer, start_link, [ChildId, MaxChildren, self(), ChildMFA]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?PoolServer]
    },
    {ok, {SupFlags, [ChildSpec]}}.
