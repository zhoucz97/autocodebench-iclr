-module(average_without_extremes).
-export([average_without_max_min/1, test/0]).

average_without_max_min(Numbers) ->
    Sorted = lists:sort(Numbers),
    Trimmed = lists:sublist(Sorted, 2, length(Sorted) - 2),
    Sum = lists:sum(Trimmed),
    Count = length(Trimmed),
    Sum / Count.
test() ->
3.0 = average_without_max_min([1, 2, 3, 4, 5]),
30.0 = average_without_max_min([10, 20, 30, 40, 50]),
undefined = average_without_max_min([1, 2]),
undefined = average_without_max_min([]),
5.0 = average_without_max_min([5, 5, 5, 5, 5, 5]),
ok.