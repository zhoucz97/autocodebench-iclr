-module(sum_odds).
-export([sum_odds_list/1, test/0]).

sum_odds_list(List) ->
    lists:foldl(fun(X, Acc) when X rem 2 =/= 0 -> X + Acc;
                   (_, Acc) -> Acc
                end, 0, List).
test() ->
9 = sum_odds_list([1, 2, 3, 4, 5]),
113 = sum_odds_list([10, 23, 35, 42, 55]),
0 = sum_odds_list([2, 4, 6, 8]),
1 = sum_odds_list([1]),
ok.