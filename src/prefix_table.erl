-module(prefix_table).

-define(START_ENTRY, '__prefix_table_start__').
-define(STOP_ENTRY, '__prefix_table_stop__').

%% Dialyzer doesn't properly support maps, so we are using dicts.
-opaque table(_Entry) :: {pos_integer(), list(), dict:dict()}.

-export_type([table/1]).

-export([ new/1
        , add/2
        , finish/1
        , merge/2
        , generate_entries/1
        ]).

-spec new(pos_integer()) -> table(_).
new(PrefixLength) when PrefixLength > 0 ->
  Prefix = start_prefix(PrefixLength),
  {PrefixLength, Prefix, dict:new()}.

-spec add(table(Entry), Entry) -> table(Entry).
add({PrefixLength, Prefix0, Dict0}, Entry) ->
  Dict = dict:append(Prefix0, Entry, Dict0),
  Prefix = update_prefix(Prefix0, Entry),
  {PrefixLength, Prefix, Dict}.

-spec finish(table(Entry)) -> table(Entry).
finish({PrefixLength, Prefix, Dict0}) ->
  Dict = dict:append(Prefix, ?STOP_ENTRY, Dict0),
  {PrefixLength, [], Dict}.

-spec merge(table(Entry), table(Entry)) -> table(Entry).
merge({PL, [], DictA}, {PL, [], DictB}) ->
  MergeFun = fun(_Prefix, V1, V2) -> V1 ++ V2 end,
  MergedDict = dict:merge(MergeFun, DictA, DictB),
  {PL, [], MergedDict}.

-spec generate_entries(table(Entry)) -> [Entry].
generate_entries({PL, [], Dict}) ->
  generate_entries(Dict, start_prefix(PL));
generate_entries({_, _, _})      ->
  error({badarg, table_not_finished}).

%% Internal
generate_entries(Dict, Prefix) ->
  case generate_entry(Dict, Prefix) of
    ?STOP_ENTRY -> [];
    Entry       ->
      [Entry|generate_entries(Dict, update_prefix(Prefix, Entry))]
  end.

generate_entry(Dict, Prefix) ->
  Available = dict:fetch(Prefix, Dict),
  Index = random:uniform(length(Available)),
  lists:nth(Index, Available). 

start_prefix(PrefixLength) -> lists:duplicate(PrefixLength, ?START_ENTRY).
update_prefix([_|Tail], Last) -> Tail ++ [Last].
