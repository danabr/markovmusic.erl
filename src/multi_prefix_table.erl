-module(multi_prefix_table).

-export([ from_list/1
        , merge/2
        , entries/2
        ]).

from_list(L) ->
  from_list(L, [], []).

from_list([], _, Table) -> dict:from_list(Table);
from_list([P|Rest], Prefix, Table) ->
  from_list(Rest, Prefix ++ [P], [{Prefix, P}|Table]).

merge(T1, T2) ->
  OnConflict = fun(_, A, B) -> lists:usort(lists:flatten([A, B])) end,
  dict:merge(OnConflict, T1, T2).

entries(Table, Prefix) ->
  case dict:find(Prefix, Table) of
    {ok, E} when is_list(E) -> E;
    {ok, E}                 -> [E];
    error                   -> []
  end.
