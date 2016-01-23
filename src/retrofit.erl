-module(retrofit).

-export([song/2]).

-spec song(midi:song(), midi:song()) -> midi:song().
song({midi, _}=Melody, {midi, {_, TimeSpec, _}}=Tempo) ->
  Table = multi_prefix_table:from_list(keys(Melody)),
  KS = midi:key_signature(Melody),
  NewNotes = generate(midi:notes(Tempo), [], [], Table),
  midi:from_notes(NewNotes, TimeSpec, KS).

%% Internal
generate([], Generated, _Prefix, _Table)                   ->
  lists:reverse(Generated);
generate([{note, _, D}=N|Notes], Generated, Prefix, Table) ->
  case multi_prefix_table:entries(Table, Prefix) of
    []       -> generate([N|Notes], Generated, [], Table);
    [NewKey] ->
      generate(Notes, [{note, NewKey, D}|Generated], Prefix ++ [NewKey], Table)
  end.

keys(Song) -> [ K || {note, K, _} <- midi:notes(Song) ].
