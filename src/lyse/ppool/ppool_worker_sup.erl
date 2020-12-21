-module(ppool_worker_sup).

-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

% public API

-spec start_link(types:mfargs()) -> supervisor:startlink_ret().
start_link(ChildMFA) ->
    supervisor:start_link(?MODULE, ChildMFA).

% supervisor callbacks

init({M, F, A}) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 3600
    },
    ChildSpec = #{
        id => ppool_worker,
        start => {M, F, A},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [M]
    },
    {ok, {SupFlags, [ChildSpec]}}.
