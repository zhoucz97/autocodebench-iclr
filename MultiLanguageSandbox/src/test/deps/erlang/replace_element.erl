-module(replace_element).
-export([replace_in_list/3, test/0]).

replace_in_list(List, Index, NewElement) ->
    {Before, [_ | After]} = lists:split(Index - 1, List),
    Before ++ [NewElement | After].
test() ->
[1, 2, 99, 4, 5] = replace_in_list([1, 2, 3, 4, 5], 3, 99),
["z", "b", "c", "d"] = replace_in_list(["a", "b", "c", "d"], 1, "z"),
ok.