-module(minimum_n).
-export([find_min_n/1, test/0]).

find_min_n(K, N, Sum) ->
    NewSum = Sum + 1 / N,
    find_min_n(K, N + 1, NewSum).

test() ->
    2 = find_min_n(1),
    4 = find_min_n(2),
    11 = find_min_n(3),
    31 = find_min_n(4),
    83 = find_min_n(5),
    ok.