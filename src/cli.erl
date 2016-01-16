-module(cli).

-export([main/1]).

main(Args) ->
  run_command(parse_args(Args)).

run_command({analyze, Opts, Files}) ->
  PrefixLength = proplists:get_value(prefix_length, Opts, 4),
  OutFile = proplists:get_value(output_file, Opts, "midi.analysis"),
  case markovmusic:analyze_files(PrefixLength, Files) of
    {ok, Analysis} -> write_analysis_file(OutFile, Analysis);
    {error, E}     ->
      io:format("Analysis failed: ~p~n.", [E])
  end;
run_command({help, undefined})                 -> print_usage();
run_command({help, Error}) when is_list(Error) ->
  io:format("Error: ~s~n", [Error]),
  print_usage().

write_analysis_file(OutFile, Analysis) ->
  Bin = term_to_binary(Analysis),
  case file:write_file(OutFile, Bin) of
    ok ->
      io:format("Analysis file written to ~s.~n", [OutFile]);
    Err ->
      io:format("Failed to write file ~s: ~p.~n", [OutFile, Err])
  end.

parse_args(["analyze"|Args]) ->
  case analyze_args(Args, []) of
    {error, Error}      -> help(Error);
    {ok, {Opts, Files}} -> {analyze, Opts, Files}
  end;
parse_args([A|_]) -> help("Unknwon command " ++ A);
parse_args([])    -> help().

analyze_args(["--prefix-length", LengthStr | Args], Opts) ->
  try  
    Length = list_to_integer(LengthStr),
    case Length > 0 of
      false -> throw(badarg);
      true  -> analyze_args(Args, [{prefix_length, Length}|Opts])
    end
  catch
    _:badarg ->
      {error, "Prefix length must be a positive integer."}
  end;
analyze_args(["--" ++ Opt|_Args], _Opts)                  ->
  {error, "Unknown argument " ++ Opt ++ "."};
analyze_args([_|_]=Files, Opts)                           ->
  {ok, {Opts, Files}};
analyze_args([], _Opts)                                   ->
  {error, "No MIDI files given."}.


help() -> {help, undefined}.
help(Error) -> {help, Error}.

print_usage() ->
  Lines = [ "Usage:"
          , program() ++ " analyze [--prefix-length n] <midi files>"
          ],
  Print = fun(Line) -> io:format("~s~n", [Line]) end,
  lists:foreach(Print, Lines).

program() -> "run.escript".
