-module(ranking_comparison).
-export([compare_rankings/4, test/0]).

compare_rankings(A, B, C, D) ->
    Sum = A + B + C + D,
    if
        Sum < 51 -> 'Rabbit wins';
        true -> 'Rabbit lose'
    end.

test() ->
'Rabbit wins' = compare_rankings(5, 6, 7, 8),
'Rabbit wins' = compare_rankings(10, 10, 10, 10),
'Rabbit wins' = compare_rankings(3, 3, 3, 3),
'Rabbit lose' = compare_rankings(12, 13, 14, 15),
'Rabbit lose' = compare_rankings(50, 1, 1, 1),
ok.