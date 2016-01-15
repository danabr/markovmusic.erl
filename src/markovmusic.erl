-module(markovmusic).

-define(PREFIX_LENGTH, 5).

-export([main/1]).

main(Files) ->
  random:seed(erlang:now()),
  Songs = load(Files),
  Generated = generate_midi(analyze(Songs)),
  WriteF = fun({TD, Midi}) ->
    Bin = midi_generate:binary(Midi),
    Path = "/tmp/generated_" ++ integer_to_list(TD) ++ ".midi",
    ok = file:write_file(Path, Bin),
    io:format("MIDI file written to ~s~n.", [Path])
  end,
  lists:foreach(WriteF, Generated).

%% Internal
load([])           -> [];
load([File|Files]) ->
  case midi_parse:file(File) of
    {ok, Midi}       ->
      [Midi|load(Files)];
    {parse_error, _} ->
      io:format("Failed to parse ~p.~n", [File]),
      load(Files);
    {error, _}       ->
      io:format("Failed to open ~p.~n", [File]),
      load(Files)
  end.

%% Analyze a collection of midi songs.
%% Produces a list of probability chains, one per time division
%% value detected in the midi files.
analyze(Songs) ->
  Groups = group(Songs),
  [analyze_frequencies(Group, ?PREFIX_LENGTH) || Group <- Groups].

-spec group([midi:song()]) -> [{midi:time_division(), [midi:song()]}].
group([])                                         -> [];
group([{midi, {_, TimeDivision, _}}=Pivot|Songs]) ->
  Criteria = fun({midi, {_, TD, _}}) -> TD =:= TimeDivision end,
  {Same, Different} = lists:partition(Criteria, Songs),
  [{TimeDivision, [Pivot|Same]}|group(Different)].

analyze_frequencies({TimeDivision, Songs}, PrefixLength) ->
  Tracks = tracks(Songs),
  Results = [analyze_track_frequencies(Track, PrefixLength) || Track <- Tracks],
  {TimeDivision, merge(Results)}.

tracks(Songs) -> lists:flatten([ music_tracks(Song) || Song <- Songs ]).

music_tracks({midi, {_, _, Tracks}}) ->
  lists:filter(fun is_music_track/1, Tracks).

is_music_track({track, Events}) ->
  Filter = fun({event, _, {note_on, _, _, _}}) -> true;
              (_)                              -> false
           end,
  lists:any(Filter, Events).

analyze_track_frequencies({track, Events}, PrefixLength) ->
  Table0 = prefix_table:new(PrefixLength),
  FoldF = fun(Event, Table) -> prefix_table:add(Table, Event) end,
  Table = lists:foldl(FoldF, Table0, Events),
  prefix_table:finish(Table).

merge([])             -> [];
merge([Table|Tables]) ->
  lists:foldl(fun prefix_table:merge/2, Table, Tables).

generate_midi(Groups) ->
  [ {TD, generate_midi(TD, Table)} || {TD, Table} <- Groups ].

generate_midi(TimeDivision, PrefixTable) ->
  Events = prefix_table:generate_entries(PrefixTable),
  {midi, {1, TimeDivision, [{track, Events}]}}.

