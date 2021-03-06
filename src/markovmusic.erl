-module(markovmusic).

-export([ analyze_files/2
        , analyze_songs/2
        , generate/1
        , invert/1
        ]).

-type alternative() :: {alternative, midi:time_division(),
                        midi:key_signature(), markov_table:table(_)}.
-type analysis() :: {analysis, [alternative()]}.

-spec analyze_files(pos_integer(), [string()]) ->
        {ok, analysis()} | {error, term()}.
analyze_files(PrefixLength, [_|_]=Files) ->
  case load(Files) of
    {ok, Songs}      -> {ok, analyze_songs(PrefixLength, Songs)};
    {error, _}=Error -> Error
  end.

-spec analyze_songs(pos_integer(), [midi:song()]) -> analysis().
analyze_songs(PrefixLength, [_|_]=Songs) ->
  Groups = group(Songs),
  Alternatives = [analyze(PrefixLength, Group) || Group <- Groups],
  {analysis, Alternatives}.

-spec generate(analysis()) -> midi:song().
generate({analysis, Alternatives}) ->
  Nth = random:uniform(length(Alternatives)),
  SelectedAlternative = lists:nth(Nth, Alternatives),
  {alternative, TimeDivision, KeySignature, Table} = SelectedAlternative,
  MusicEvents = markov_table:generate_entries(Table),
  MetaEvents = [ {event, 0, {meta, 89, KeySignature}}
               , {event, 0, {meta, 47, <<>>}}
               ],
  Tracks = [{track, MetaEvents}, {track, MusicEvents}],
  {midi, {1, TimeDivision, Tracks}}.

-spec invert(midi:song()) -> midi:song().
invert({midi, {Format, TimeDivision, Tracks}}) ->
  NewTracks = lists:map(fun invert_track/1, Tracks),
  {midi, {Format, TimeDivision, NewTracks}}.

%% Internal
load(Files) -> load(Files, []).

load([], Songs)           -> {ok, Songs};
load([File|Files], Songs) ->
  case midi_parse:file(File) of
    {ok, Song}       -> load(Files, [Song|Songs]);
    {parse_error, _} -> {error, {parse_error, File}};
    {error, _}       -> {error, {read_error, File}}
  end.

group(Songs) ->
  SongsAndSignatures = [ {midi:key_signature(Song), Song} || Song <- Songs],
  group_by_time_signature_and_key_signature(SongsAndSignatures).

group_by_time_signature_and_key_signature([])            -> [];
group_by_time_signature_and_key_signature([Pivot|Songs]) ->
  {KeySignature, Song} = Pivot,
  {midi, {_, TimeDivision, _}} = Song,
  Matches = fun({KS, {midi, {_, TD, _}}}) ->
              KS =:= KeySignature andalso TD =:= TimeDivision
             end,
  {Same, Different} = lists:partition(Matches, Songs),
  SimilarSongs = [S || {_, S} <- Same],
  Group = {TimeDivision, KeySignature, [Song|SimilarSongs]},
  [Group|group_by_time_signature_and_key_signature(Different)].

-spec analyze(pos_integer(), {midi:time_division(), midi:key_signature(),
                              [midi:song()]}) -> alternative().
analyze(PrefixLength, {TimeDivision, KeySignature, Songs}) ->
  Table = analyze_frequencies(PrefixLength, Songs),
  {alternative, TimeDivision, KeySignature, Table}.

-spec analyze_frequencies(pos_integer(), [midi:song()]) ->
  markov_table:table(_).
analyze_frequencies(PrefixLength, Songs) ->
  Tracks = tracks(Songs),
  Tables = [analyze_track_frequencies(Track, PrefixLength) || Track <- Tracks],
  merge(Tables).

tracks(Songs) -> lists:flatten([ midi:music_tracks(Song) || Song <- Songs ]).

analyze_track_frequencies({track, Events}, PrefixLength) ->
  Table0 = markov_table:new(PrefixLength),
  FoldF = fun(Event, Table) -> markov_table:add(Table, Event) end,
  Table = lists:foldl(FoldF, Table0, filter_events(Events)),
  markov_table:finish(Table).

merge([])             -> [];
merge([Table|Tables]) ->
  lists:foldl(fun markov_table:merge/2, Table, Tables).

filter_events([]) -> [];
filter_events([{event, O, {meta, 5, _}}|Events]) ->
  [{event, O, {meta, 5, <<>>}}|filter_events(Events)];
filter_events([Event|Events]) ->
  [Event|filter_events(Events)].

invert_track({track, Events}) ->
  {track, lists:map(fun invert_event/1, Events)}.

invert_event({event, Offset, {note_on, Channel, Note, Velocity}})  ->
  NewNote = invert_note(Note),
  {event, Offset, {note_on, Channel, NewNote, Velocity}};
invert_event({event, Offset, {note_off, Channel, Note, Velocity}}) ->
  NewNote = invert_note(Note),
  {event, Offset, {note_off, Channel, NewNote, Velocity}};
invert_event({event, Offset, {meta, 89, <<SF:8/signed, MajMin>>}}) ->
  % Deal with sharps and flats
  Neg = 0 - SF,
  {event, Offset, {meta, 89, <<Neg:8/signed, MajMin>>}};
invert_event(Event)                                                ->
  Event.

invert_note(N) ->
  NoteNumber = N rem 12,
  Octave = N - NoteNumber,
  invert_octave(Octave) + invert_note_number(NoteNumber).

invert_note_number(0) -> 11;
invert_note_number(1) -> 10;
invert_note_number(2) -> 9;
invert_note_number(3) -> 8;
invert_note_number(4) -> 7;
invert_note_number(5) -> 6;
invert_note_number(6) -> 5;
invert_note_number(7) -> 4;
invert_note_number(8) -> 3;
invert_note_number(9) -> 2;
invert_note_number(10) -> 1;
invert_note_number(11) -> 0.

invert_octave(0)   -> 120;
invert_octave(12)  -> 108;
invert_octave(24)  -> 96;
invert_octave(36)  -> 84;
invert_octave(48)  -> 72;
invert_octave(60)  -> 60;
invert_octave(72)  -> 48;
invert_octave(84)  -> 36;
invert_octave(96)  -> 24;
invert_octave(108) -> 12;
invert_octave(120) -> 0.
