-module(reverse_after_position).
-export([reverse_after/2, test/0]).

reverse_after(List, Position) when Position >= 0 ->
    {Before, After} = lists:split(Position + 1, List),
    Before ++ lists:reverse(After).
test() ->
[1, 2, 3, 5, 4] = reverse_after([1, 2, 3, 4, 5], 3),
["a", "b", "e", "d", "c"] = reverse_after(["a", "b", "c", "d", "e"], 2),
[1, 2, 3, 4, 5] = reverse_after([1, 2, 3, 4, 5], 5),
[1] = reverse_after([1], 1),
["z", "y", "x"] = reverse_after(["x", "y", "z"], 0),
ok.