-module(insert_element).
-export([insert_in_list/3, test/0]).

insert_in_list(List, Position, Element) ->
    {Before, After} = lists:split(Position, List),
    Before ++ [Element | After].
test() ->
[1, 2, 3, 99, 4, 5] = insert_in_list([1, 2, 3, 4, 5], 3, 99),
["a", "b", "z", "c", "d"] = insert_in_list(["a", "b", "c", "d"], 2, "z"),
ok.