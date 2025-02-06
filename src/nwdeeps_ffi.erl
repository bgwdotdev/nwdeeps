-module(nwdeeps_ffi).
-export([open/1, get_line/1, clear_terminal/0]).

open(Path) ->
  file:open(Path, [read, binary]).

get_line(File) ->
  case io:get_line(File, '') of
    eof -> {ok, eof};
    {error, Reason } -> {error, Reason};
    Data -> {ok, {data, Data}}
  end.

clear_terminal() ->
  io:format("\e[2J\e[H").

