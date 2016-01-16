-module(markovmusic).

-export([ analyze_files/2
        , analyze_songs/2
        , generate/1
        ]).

-type analysis() :: {analysis, midi:time_division(), prefix_table:table(_)}.

-spec analyze_files(pos_integer(), [string()]) ->
        {ok, analysis()} | {error, term()}.
analyze_files(PrefixLength, [_|_]=Files) ->
  case load(Files) of
    {ok, Songs}      -> analyze_songs(PrefixLength, Songs);
    {error, _}=Error -> Error
  end.

-spec analyze_songs(pos_integer(), [midi:song()]) ->
        {ok, analysis()} | {error, different_time_division}.
analyze_songs(PrefixLength, [_|_]=Songs) ->
  case all_same_time_division(Songs) of
    true  -> {ok, do_analyze(PrefixLength, Songs)};
    false -> {error, different_time_division}
  end.

-spec generate(analysis()) -> midi:song().
generate({analysis, TimeDivision, PrefixTable}) ->
  Events = prefix_table:generate_entries(PrefixTable),
  {midi, {1, TimeDivision, [{track, Events}]}}.

%% Internal
load(Files) -> load(Files, []).

load([], Songs)           -> {ok, Songs};
load([File|Files], Songs) ->
  case midi_parse:file(File) of
    {ok, Song}       -> load(Files, [Song|Songs]);
    {parse_error, _} -> {error, {parse_error, File}};
    {error, _}       -> {error, {read_error, File}}
  end.

all_same_time_division(Songs) ->
  {midi, {_, TimeDivision, _}} = hd(Songs),
  Criteria = fun({midi, {_, TD, _}}) -> TD =:= TimeDivision end,
  lists:all(Criteria, Songs).

-spec do_analyze(pos_integer(), [midi:song()]) -> analysis().
do_analyze(PrefixLength, Songs) ->
  {midi, {_, TimeDivision, _}} = hd(Songs),
  Table = analyze_frequencies(PrefixLength, Songs),
  {analysis, TimeDivision, Table}.

analyze_frequencies(PrefixLength, Songs) ->
  Tracks = tracks(Songs),
  Tables = [analyze_track_frequencies(Track, PrefixLength) || Track <- Tracks],
  merge(Tables).

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

