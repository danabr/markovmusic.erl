-module(cli).

-export([main/1]).

main(Args) ->
  try run_command(parse_args(Args))
  catch
    throw:{read_error, File} ->
      io:format("Failed to read ~s.~n", [File]);
    throw:{parse_error, File} ->
      io:format("Failed to parse ~s as a MIDI file.~n", [File])
  end.

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
run_command({invert, MidiFile, OutFile})       ->
  {ok, Song} = midi_parse:file(MidiFile),
  Inverted = markovmusic:invert(Song),
  write_midi(OutFile, Inverted);
run_command({retrofit, MelodyFile, TempoFile, OutFile}) ->
  Melody = open_song(MelodyFile),
  Tempo = open_song(TempoFile),
  Song = retrofit:song(Melody,  Tempo),
  write_midi(OutFile, Song);
run_command({help, undefined})                 -> print_usage();
run_command({help, Error}) when is_list(Error) ->
  io:format("Error: ~s~n", [Error]),
  print_usage().

open_song(File) ->
  case midi_parse:file(File) of
    {ok, Song}       -> Song;
    {parse_error, _} -> throw({parse_error, File});
    {error, _}       -> throw({read_error, File})
  end.

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
    {analysis, _}=Analysis -> {ok, Analysis};
    _                      -> {error, parse_error}
  catch
    _:badarg -> {error, parse_error}
  end.

parse_args(["analyze"|Args])                            ->
  case analyze_args(Args, []) of
    {error, Error}      -> help(Error);
    {ok, {Opts, Files}} -> {analyze, Opts, Files}
  end;
parse_args(["generate", AnalysisFile, OutFile])         ->
  {generate, AnalysisFile, OutFile};
parse_args(["invert", MidiFile, OutFile])               ->
  {invert, MidiFile, OutFile};
parse_args(["retrofit", MusicFile, TempoFile, OutFile]) ->
  {retrofit, MusicFile, TempoFile, OutFile};
parse_args(_)                                           ->
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
          , program() ++ " invert <midi-file> <output-file>"
          , program() ++ " retrofit <melody-file> <tempo-file> <out-file>"
          ],
  Print = fun(Line) -> io:format("~s~n", [Line]) end,
  lists:foreach(Print, Lines).

program() -> "run.escript".
