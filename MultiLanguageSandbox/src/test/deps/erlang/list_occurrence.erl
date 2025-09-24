-module(list_occurrence).
-export([find_list_in_list/2, test/0]).

find_list_in_list(A, [H|T], Pos) ->
    case lists:prefix(A, [H|T]) of
        true -> Pos;
        false -> find_list_in_list(A, T, Pos + 1)
    end.
test() ->
1 = find_list_in_list([2, 3], [1, 2, 3, 4]),
-1 = find_list_in_list([3, 4, 5], [1, 2, 3]),
-1 = find_list_in_list([1, 2, 3], []),
0 = find_list_in_list([], [1, 2, 3]),
-1 = find_list_in_list([1, 2, 3], [4, 5, 6]),
ok.