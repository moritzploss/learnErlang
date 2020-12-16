-module(ev_sup).

-export([start/2, start_link/2]).
-export([init/1]).

start(Mod, Args) ->
    spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod, Args) ->
    spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
    process_flag(trap_exit, true),
    loop({Mod, start_link, Args}).

loop({Mod, Fun, Args}) ->
    Pid = apply(Mod, Fun, Args),
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, _Reason} ->
            loop({Mod, Fun, Args})
    end.
