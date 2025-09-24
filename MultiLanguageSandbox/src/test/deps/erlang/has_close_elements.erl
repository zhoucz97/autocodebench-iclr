-module(has_close_elements).
-export([has_close_elements/2, test/0]).

has_close_elements([X, Y | Rest], Threshold) ->
    case abs(X - Y) < Threshold of
        true -> true;
        false -> has_close_elements([Y | Rest], Threshold)
    end.
test() ->
true = has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3),
false = has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05),
true = has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95),
false = has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8),
true = has_close_elements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1),
true = has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0),
false = has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5),
ok.