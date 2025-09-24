-module(quick_sort).
-export([quick_sort/1, test/0]).

quick_sort([Pivot|Rest]) ->
    {Lesser, Greater} = partition(Pivot, Rest, [], []),
    quick_sort(Lesser) ++ [Pivot] ++ quick_sort(Greater).

partition(_, [], Lesser, Greater) -> {Lesser, Greater};
partition(Pivot, [H|T], Lesser, Greater) when H =< Pivot ->
    partition(Pivot, T, [H|Lesser], Greater);
partition(Pivot, [H|T], Lesser, Greater) ->
    partition(Pivot, T, Lesser, [H|Greater]).
test() ->
[1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9] = quick_sort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]),
[1, 2, 3, 4, 5] = quick_sort([5, 4, 3, 2, 1]),
[] = quick_sort([]),
[1] = quick_sort([1]),
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = quick_sort([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]),
ok.