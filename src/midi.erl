-module(midi).

-type song() :: {midi, {format(), time_division(), [track()]}}.
-type format() :: 0 | 1 | 2.
-type time_division() :: non_neg_integer().
-type track() :: {track, [event()]}.
-type event() :: {event, offset(), event_data()}.
-type event_data() :: {meta, meta_event_type(), Data::binary()} |
                      {note_off, channel(), note(), velocity()} |
                      {note_on, channel(), note(), velocity()} |
                      {controller_event, channel(), controller_type(),
                                         controller_value()} |
                      {program_change, channel(), program_number()}.
-type offset() :: non_neg_integer().
-type meta_event_type() :: byte().
-type channel() :: 0-15.
-type note() :: 0-127.
-type velocity() :: 0-127.
-type controller_type() :: 0-127.
-type controller_value() :: 0-127.
-type program_number() :: 0-127.
-type key_signature() :: binary().

-export_type([ channel/0
             , controller_type/0
             , controller_value/0
             , event/0
             , event_data/0
             , format/0
             , key_signature/0
             , meta_event_type/0
             , note/0
             , offset/0
             , program_number/0
             , song/0
             , time_division/0
             , track/0
             , velocity/0
             ]).

-export([ key_signature/1
        , music_tracks/1
        ]).

-spec key_signature(song()) -> key_signature().
key_signature({midi, {_, _, Tracks}}) ->
  key_signature_from_tracks(Tracks).

-spec music_tracks(song()) -> [track()].
music_tracks({midi, {_, _, Tracks}}) ->
  lists:filter(fun is_music_track/1, Tracks).

%% Internal

is_music_track({track, Events}) ->
  Filter = fun({event, _, {note_on, _, _, _}}) -> true;
              (_)                              -> false
           end,
  lists:any(Filter, Events).


key_signature_from_tracks([]) -> <<0,0>>;
key_signature_from_tracks([{track, Events}|Tracks]) ->
  case key_signature_from_events(Events) of
    {ok, Signature}  -> Signature;
    no_key_signature -> key_signature_from_tracks(Tracks)
  end.

key_signature_from_events([]) -> no_key_signature;
key_signature_from_events([{event, _, {meta, 89, Sig}}|_]) -> {ok, Sig};
key_signature_from_events([_|Events]) ->
  key_signature_from_events(Events).
