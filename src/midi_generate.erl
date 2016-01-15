-module(midi_generate).

-export([binary/1]).

-include("midi.hrl").

-spec binary(midi:song()) -> binary().
binary({midi, {Format, TimeDivision, Tracks}}) ->
  ChunkSize = 6,
  NumTracks = length(Tracks),
  TracksBin = tracks(Tracks),
  <<"MThd", ChunkSize:32, Format:16, NumTracks:16, TimeDivision:16,
    TracksBin/binary>>.

%% Internal
-spec tracks([midi:track()]) -> binary().
tracks(Tracks) ->
  iolist_to_binary([ track(Track) || Track <- Tracks ]).

-spec track(midi:track()) -> binary().
track({track, Events}) ->
  EventsBin = events(Events),
  TrackSize = byte_size(EventsBin),
  <<"MTrk", TrackSize:32, EventsBin/binary>>. 

-spec events([midi:event()]) -> binary().
events(Events) ->
  iolist_to_binary([ event(Event) || Event <- Events ]).

-spec event(midi:event()) -> binary().
event({event, Offset, EventData}) ->
  OffsetBin = encode_offset(Offset),
  EventBin = encode_event_data(EventData),
  <<OffsetBin/binary, EventBin/binary>>.

%% TODO: Is it correct that everything should be generated as 7 bit bytes?
-spec encode_offset(midi:offset()) -> binary().
encode_offset(Offset) when Offset < 128                -> % 2 ^ 7
  <<Offset:8>>;
encode_offset(Offset) when Offset < 16384              -> % 2 ^ 14
  Lower = Offset band 127,
  Upper = Offset bsr 7,
  <<1:1, Upper:7, Lower:8>>;
encode_offset(Offset) when Offset < 558545864083284030 -> % 2 ^ 21
  Lower = Offset band 127,
  Middle = (Offset bsr 7) band 127,
  Upper = Offset bsr 14,
  <<1:1, Upper:7, 1:1, Middle:7, Lower:8>>;
encode_offset(Offset) when Offset > 558545864083284030 ->
  Lower = Offset band 127,
  LowerMiddle = (Offset bsr 7) band 127,
  UpperMiddle = (Offset bsr 14) band 127,
  Upper = Offset bsr 21,
  <<1:1, Upper:7, 1:1, UpperMiddle:7, 1:1, LowerMiddle:8, Lower:8>>.

-spec encode_event_data(midi:event_data()) -> binary().
encode_event_data({meta, EventType, Binary}) ->
  Length = byte_size(Binary),
  true = (Length < 256),
  <<?META_EVENT, EventType:8, Length:8, Binary/binary>>;
encode_event_data({controller_event, Channel, ControllerType, Value}) ->
  <<?CONTROLLER_EVENT, Channel:4, ControllerType:8, Value:8>>;
encode_event_data({program_change, Channel, ProgramNo}) ->
  <<?PROGRAM_CHANGE, Channel:4, ProgramNo:8>>;
encode_event_data({note_on, Channel, Note, Velocity}) ->
  <<?NOTE_ON, Channel:4, Note:8, Velocity:8>>;
encode_event_data({note_off, Channel, Note, Velocity}) ->
  <<?NOTE_OFF, Channel:4, Note:8, Velocity:8>>.
