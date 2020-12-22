-module(learn_erlang).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(normal, _Args) ->
    ppool_manager:start_link().

stop(_State) ->
    ok.
