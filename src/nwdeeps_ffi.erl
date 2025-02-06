-module(nwdeeps_ffi).
-export([open/1, get_line/1]).

open(Path) ->
  file:open(Path, [read, binary]).

get_line(File) ->
  io:get_line(File, '').
