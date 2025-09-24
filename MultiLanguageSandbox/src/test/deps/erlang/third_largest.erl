-module(third_largest).
-export([find_third_largest/1, test/0]).

find_third_largest(Numbers) ->
    UniqueSorted = lists:usort(Numbers),
    case length(UniqueSorted) of
        Len when Len >= 3 ->
            lists:nth(3, UniqueSorted);
        _ ->
            hd(UniqueSorted)
    end.
test() ->
1 = find_third_largest([2, 3, 1]),
3 = find_third_largest([9, 2, 3, 6]),
5 = find_third_largest([5]),
7 = find_third_largest([7, 7, 7]),
ok.