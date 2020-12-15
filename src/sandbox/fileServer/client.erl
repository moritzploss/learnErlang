-module(client).

-export([ls/1, get_file/2, put_file/3]).

ls(Server) ->
    Server ! {self(), list_dir},
        receive
      {Server, FileList} ->
          FileList
    end.

get_file(Server, FileName) ->
    Server ! {self(), {get_file, FileName}},
    receive
      {Server, Content} ->
          Content
    end.

put_file(Server, Content, FileName) ->
    Server ! {self(), {put_file, {Content, FileName}}},
    receive
      {Server, ok} ->
          ok
    end.

