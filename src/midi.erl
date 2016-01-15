-module(midi).

-type song() :: {midi, {format(), time_division(), [track()]}}.
-type format() :: 0 | 1 | 2.
-type time_division() :: non_neg_integer().
-type track() :: {track, event()}.
-type event() :: {meta_event, {offset(), meta_event_type(), Data::binary()}} |
                 {note_off_event, {offset(), channel(), note(), velocity()}} |
                 {note_on_event, {offset(), channel(), note(), velocity()}} |
                 {controller_event, {offset(), channel(), controller_type(),
                                     controller_value()}} |
                 {program_change_event, {offset(), channel(), program_number()}}.
-type offset() :: non_neg_integer().
-type meta_event_type() :: byte().
-type channel() :: 0-15.
-type note() :: 0-127.
-type velocity() :: 0-127.
-type controller_type() :: 0-127.
-type controller_value() :: 0-127.
-type program_number() :: 0-127.

-type parse_error() :: {parse_error, Rsn::term()}.
-type io_error() :: {error, Rsn::term()}.

-export([parse/1]).

-spec parse(File::string()) -> {ok, song()} | parse_error() | io_error().
parse(File) when is_list(File) ->
	case file:read_file(File) of
		{ok, Bin}        -> parse_binary(Bin);
		{error, _}=Error -> Error
	end.

-spec parse_binary(binary()) -> {ok, song()} | parse_error().
parse_binary(Bin) ->
	try {ok, parse_midi(Bin)}
	catch
		throw:{parse_error,_}=E -> E
	end.

%% Internal

-spec parse_midi(binary()) -> song().
parse_midi(<<"MThd", ChunkSize:32, Format:16, NumTracks:16, TimeDivision:16,
						 Rest/binary>>) when ChunkSize =:= 6 ->
	Tracks = parse_tracks(NumTracks, Rest),
	{midi, {validate_format(Format), TimeDivision, Tracks}};
parse_midi(_)                                    ->
  parse_error(bad_midi_header).

validate_format(N) when N >= 0 andalso N < 3 -> N;
validate_format(N)                           -> parse_error({bad_format, N}).

parse_tracks(0, <<>>)                                     -> [];
parse_tracks(0, _Bin)                                     ->
  parse_error(garbage_after_tracks);
parse_tracks(N, <<"MTrk", TrackLength:32, Rest0/binary>>) ->
	case Rest0 of
		<<EventData:TrackLength/binary, Rest/binary>> ->
			Track = {track, parse_track(EventData)},
			[Track|parse_tracks(N-1, Rest)];
		_WrongSize
			-> throw({parse_error, track})
	end;
parse_tracks(_, _)                                        ->
	parse_error(track).

parse_track(<<>>) -> [];
parse_track(Bin0) ->
	{Offset, Bin1} = extract_time_offset(Bin0),
	{Event, Bin2} = parse_event(Offset, Bin1),
	[Event|parse_track(Bin2)].

extract_time_offset(<<1:1, O1:7, 1:1, O2:7, 1:1, O3:7, 0:1, O4:7,
											Bin/binary>>)                                  ->
	Offset = (O1 bsl 21) bor (O2 bsl 14) bor (O3 bsl 7) bor O4,
	{Offset, Bin};
extract_time_offset(<<1:1, O1:7, 1:1, O2:7, 0:1, O3:7, Bin/binary>>) ->
	Offset = (O1 bsl 14) bor (O2 bsl 7) bor O3,
	{Offset, Bin};
extract_time_offset(<<1:1, O1:7, 0:1, O2:7, Bin/binary>>) ->
	Offset = (O1 bsl 7) bor O2,
	{Offset, Bin};
extract_time_offset(<<0:1, Offset:7, Bin/binary>>)                   ->
	{Offset, Bin};
extract_time_offset(_)                                               ->
	parse_error(time_offset).

parse_event(Offset, <<255, Type, Length, Bin0/binary>>) ->
	parse_meta_event(Offset, Type, Length, Bin0);
parse_event(Offset, <<8:4, Channel:4, Note, Velocity, Bin0/binary>>) ->
	Event = {note_off_event, {Offset, Channel, Note, Velocity}},
	{Event, Bin0};
parse_event(Offset, <<9:4, Channel:4, Note, Velocity, Bin0/binary>>) ->
	Event = {note_on_event, {Offset, Channel, Note, Velocity}},
	{Event, Bin0};
parse_event(Offset, <<11:4, Channel:4, ControllerType, Value, Bin0/binary>>) ->
	Event = {controller_event, {Offset, Channel, ControllerType, Value}},
	{Event, Bin0};
parse_event(Offset, <<12:4, Channel:4, ProgramNo, Bin0/binary>>) ->
	Event = {program_change_event, {Offset, Channel, ProgramNo}},
	{Event, Bin0};
parse_event(Offset, <<Type:4, Channel:4, P1, P2, _Bin0/binary>>) ->
	throw({not_implemented, {Offset, Type, Channel, P1, P2}}).

parse_meta_event(Offset, Type, Length, Bin0) ->
	case Bin0 of
		<<Data:Length/binary, Bin/binary>> ->
			Event = {meta_event, {Offset, Type, Data}},
			{Event, Bin};
		_WrongSize                         ->
			parse_error(bad_meta_event_data_size)
	end.

parse_error(Error) -> throw({parse_error, Error}).
