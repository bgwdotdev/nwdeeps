-module(nwdeeps_ffi).
-export([open/1, close/1, list_dir/1, read_file_mtime/1, get_line/1, clear_terminal/0, top_terminal/0, to_ascii_int/1]).

-include_lib("kernel/include/file.hrl").

open(Path) ->
  case file:open(Path, [read, binary]) of
    {error, Reason} -> {error, {file, Reason}};
    {ok, IoDevice} -> {ok, IoDevice}
  end.

close(File) ->
  case file:close(File) of
    {error, Reason} -> {error, {file, Reason}};
    ok -> {ok, nil}
  end.

list_dir(Dir) ->
  case file:list_dir(Dir) of
    {error, Reason} -> {error, {file, Reason}};
    {ok, FileNames} -> {ok, lists:map(fun unicode:characters_to_binary/1, FileNames)}
  end.

read_file_mtime(File) ->
  case file:read_file_info(File, [{time, posix}]) of
    {error, Reason} -> {error, {file, Reason}};
    {ok, #file_info{mtime = Mtime}} -> {ok, Mtime}
  end.

get_line(File) ->
  case io:get_line(File, '') of
    {error, Reason} -> {error, {file, Reason}};
    eof -> {ok, eof};
    Data -> {ok, {data, Data}}
  end.

clear_terminal() ->
  io:format("\e[2J\e[H").

top_terminal() ->
  io:format("\033[H").

to_ascii_int(Char) ->
  hd(binary:bin_to_list(Char)).
