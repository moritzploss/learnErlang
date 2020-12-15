-module(sandbox_tests).

-include_lib("eunit/include/eunit.hrl").

double_register_test_() ->
    {setup, fun start/0, fun stop/1, fun double_register_pid/1}.

start() ->
    {ok, Pid} = registry:start_link(),
    Pid.

stop(Pid) ->
    registry:stop(Pid).

double_register_pid(Pid) ->
    ok = registry:register(Pid, unique_name, self()),
    Res = registry:register(Pid, another_unique_name, self()),
    ?_assertEqual({error, already_named}, Res).
