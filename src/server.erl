-module(server).
-export([start/1, loop/1]).

start(Dir) -> spawn(server, loop, [Dir]).

loop(Dir) ->
  receive
    {Client, list_dir} ->
      Client ! {self(), file:list_dir(Dir)};

    {Client, {get_file, FileName}} ->
      Full = filename:join(Dir, FileName),
      Client ! {self(), file:read_file(Full)};

    {Client, {put_file, Content, FileName}} ->
      Full = filename:join(Dir, FileName),
      Client ! {self(), file:write_file(Full, Content)}
  end,
  loop(Dir).
