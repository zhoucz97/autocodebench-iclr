-module(merge_lists).
-export([merge/2, test/0]).

merge(List1, List2) ->
    List1 ++ List2.
test() ->
[1, 2, 3, 4, 5, 6] = merge([1, 2, 3], [4, 5, 6]),
["apple", "banana", "cherry", "date"] = merge(["apple", "banana"], ["cherry", "date"]),
[1, 2, 3] = merge([], [1, 2, 3]),
[1, 2, 3] = merge([1, 2, 3], []),
[] = merge([], []),
ok.