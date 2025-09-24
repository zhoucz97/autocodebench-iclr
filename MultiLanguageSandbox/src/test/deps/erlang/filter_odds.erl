-module(filter_odds).
-export([filter_odds/1, test/0]).

filter_odds(List) ->
    lists:filter(fun(X) -> X rem 2 =/= 0 end, List).
test() ->
[1, 3, 5] = filter_odds([1, 2, 3, 4, 5, 6]),
[23, 35, 55] = filter_odds([10, 23, 35, 42, 55]),
[] = filter_odds([2, 4, 6, 8, 10]),
[1] = filter_odds([1]),
ok.