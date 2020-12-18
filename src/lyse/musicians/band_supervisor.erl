-module(band_supervisor).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-type mode() :: lenient | angry | strict.

-spec start_link(Mode :: mode()) -> {ok, pid()}.
start_link(Mode) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Mode).

child_config_simple() ->
    {
        jam_musician,
        {musicians, start_link, []},
        temporary,
        1000,
        worker,
        [musicians]
    }.

child_config(Restart, Role, Skill) ->
    {
        Role,
        {musicians, start_link, [Role, Skill]},
        Restart,
        1000,
        worker,
        [musicians]
    }.

init(lenient) ->
    init({one_for_one, 3, 60});
init(angry) ->
    init({rest_for_one, 2, 60});
init(strict) ->
    init({one_for_all, 1, 60});
init(jamband) ->
    {
        ok,
        {
            {simple_one_for_one, 3, 60},
            [child_config_simple()]
        }
    };
init({RestartStrategy, MaxRestarts, MaxTime}) ->
    {
        ok,
        {{RestartStrategy, MaxRestarts, MaxTime}, [
            child_config(permanent, singer, good),
            child_config(transient, drums, good),
            child_config(temporary, keys, good),
            child_config(transient, guitar, bad)
        ]}
    }.
