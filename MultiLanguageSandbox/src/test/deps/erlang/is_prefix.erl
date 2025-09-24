-module(is_prefix).
-export([is_prefix/2, test/0]).

is_prefix(List1, List2) ->
    lists:prefix(List1, List2).
test() ->
true = is_prefix([1, 2], [1, 2, 3, 4]),
false = is_prefix([1, 3], [1, 2, 3, 4]),
true = is_prefix([], [1, 2, 3, 4]),
false = is_prefix([1, 2, 3, 4], [1, 2]),
true = is_prefix([], []),
false = is_prefix([1], []),
ok.