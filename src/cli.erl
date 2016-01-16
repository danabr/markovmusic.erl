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
run_command({generate, AnalysisFile, OutFile}) ->
  random:seed(erlang:now()),
  case read_analysis_file(AnalysisFile) of
    {ok, Analysis} -> write_midi(OutFile, markovmusic:generate(Analysis));
    {error, _}     -> io:format("Failed to read analysis file.~n")
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

read_analysis_file(File) ->
  case file:read_file(File) of
    {ok, Bin}          -> parse_analysis_file(Bin);
    {error, _} = Error -> Error
  end.

write_midi(OutFile, Song) ->
  Bin = midi_generate:binary(Song),
  case file:write_file(OutFile, Bin) of
    ok         -> io:format("MIDI file written to ~s.~n", [OutFile]);
    {error, _} -> io:format("Failed to write MIDI file to ~s.~n", [OutFile])
  end.
parse_analysis_file(Bin) when is_binary(Bin) ->
  try binary_to_term(Bin) of
    {analysis, _, _}=Analysis -> {ok, Analysis};
    _                         -> {error, parse_error}
  catch
    _:badarg -> {error, parse_error}
  end.

parse_args(["analyze"|Args])                       ->
  case analyze_args(Args, []) of
    {error, Error}      -> help(Error);
    {ok, {Opts, Files}} -> {analyze, Opts, Files}
  end;
parse_args(["generate", AnalysisFile, OutFile])    ->
  {generate, AnalysisFile, OutFile};
parse_args(["generate"|_])                         ->
  help();
parse_args([A|_])                                  ->
  help("Unknwon command " ++ A);
parse_args([])                                     ->
  help().

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
analyze_args(["--out", OutFile | Args], Opts)             ->
  analyze_args(Args, [{output_file, OutFile}|Opts]);
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
          , program() ++ " analyze [--prefix-length <n>] "
                         " [--out <analysis-file>] <midi files>"
          , program() ++ " generate <analysis-file> <output-file>"
          ],
  Print = fun(Line) -> io:format("~s~n", [Line]) end,
  lists:foreach(Print, Lines).

program() -> "run.escript".
