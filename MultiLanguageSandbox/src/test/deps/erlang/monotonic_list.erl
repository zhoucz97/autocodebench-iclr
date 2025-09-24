-module(monotonic_list).
-export([is_monotonic/1, test/0]).

is_monotonic(List) ->
    case List of
        [] -> true;  % Empty list is considered monotonic
        [_] -> true; % Single-element list is considered monotonic
        _ ->
            NonDecreasing = is_non_decreasing(List),
            NonIncreasing = is_non_increasing(List),
            NonDecreasing orelse NonIncreasing
    end.

is_non_decreasing([_]) -> true;
is_non_decreasing([X, Y | Rest]) when X =< Y -> is_non_decreasing([Y | Rest]);
is_non_decreasing(_) -> false.

is_non_increasing([_]) -> true;
is_non_increasing([X, Y | Rest]) when X >= Y -> is_non_increasing([Y | Rest]);
is_non_increasing(_) -> false.
test() ->
    true = is_monotonic([1, 2, 2, 3]),
    true = is_monotonic([3, 2, 1]),
    false = is_monotonic([1, 3, 2]),
    true = is_monotonic([10, 10, 10]),
    true = is_monotonic([5]),
    true = is_monotonic([]),
    false = is_monotonic([1, 2, 3, 2, 4]),
    ok.