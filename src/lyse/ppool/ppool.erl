-module(ppool).

-export_type([child_Id/0]).

-type child_Id() :: atom() | string().

-export([
    start_link/0,
    stop/0,
    start_pool/3,
    run/2,
    sync_queue/2,
    async_queue/2,
    stop_pool/1
]).

start_link() ->
    ppool_manager:start_link().

stop() ->
    ppool_manager:stop().

start_pool(Name, Limit, {M, F, A}) ->
    ppool_manager:start_pool(Name, Limit, {M, F, A}).

stop_pool(Name) ->
    ppool_manager:stop_pool(Name).

run(Name, Args) ->
    ppool_serv:run(Name, Args).

async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).

sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).
