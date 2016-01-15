-module(midi).

-export([parse/1]).

parse(File) when is_list(File) ->
	case file:read_file(File) of
		{ok, Bin}        -> parse_binary(Bin);
		{error, _}=Error -> Error
	end.

parse_binary(Bin) ->
	try parse_midi(Bin)
	catch	
		throw:{parse_error,_}=E -> E
	end.

parse_midi(<<"MThd", ChunkSize:32, Format:16, NumTracks:16, TimeDivision:16,
						 Rest/binary>>) when ChunkSize =:= 6 ->
	Tracks = parse_tracks(NumTracks, Rest),
	{ok, {Format, Tracks, TimeDivision}};
parse_midi(_) ->
	{error, {parse_error, bad_midi_header}}.

parse_tracks(0, <<>>) -> [];
parse_tracks(0, _Bin) -> throw({parse_error, garbage_after_tracks});
parse_tracks(N, <<"MTrk", TrackLength:32, Rest0/binary>>) ->
	case Rest0 of	
		<<EventData:TrackLength/binary, Rest/binary>> ->
			Track = {track, parse_track(EventData)},
			[Track|parse_tracks(N-1, Rest)];
		_WrongSize
			-> throw({parse_error, track})
	end;
parse_tracks(_, _) ->
	throw({parse_error, track}).

parse_track(<<>>) -> [];
parse_track(Bin0) ->
	{Offset, Bin1} = extract_time_offset(Bin0),
	{Event, Bin2} = parse_event(Offset, Bin1),
	[Event|parse_track(Bin2)].	

extract_time_offset(<<1:1, O1:7, 1:1, O2:7, 1:1, O3:7, 0:1, O4:7,
											Bin/binary>>) ->
	Offset = (O1 bsl 21) band (O2 bsl 14) band (O3 bsl 7) band O4,
	{Offset, Bin};
extract_time_offset(<<1:1, O1:7, 1:1, O2:7, 0:1, O3:7, Bin/binary>>) ->
	Offset = (O1 bsl 14) band (O2 bsl 7) band O3,
	{Offset, Bin};
extract_time_offset(<<1:1, O1:7, 0:1, O2:7, Bin/binary>>) ->
	Offset = (O1 bsl 7) band O2,
	{Offset, Bin};
extract_time_offset(<<0:1, Offset:7, Bin/binary>>) ->
	{Offset, Bin};
extract_time_offset(_) ->
	throw({parse_error, time_offset}).

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
		_                             ->
			throw({parse_error, bad_meta_event_data_size})
	end.
