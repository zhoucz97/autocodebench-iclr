-module(swimming_distance).
-export([total_swim_distance/2, test/0]).

total_swim_distance(Day, RemainingDays, Acc) ->
    case Day of
        6 -> % Saturday
            total_swim_distance(7, RemainingDays - 1, Acc);
        7 -> % Sunday
            total_swim_distance(1, RemainingDays - 1, Acc);
        _ -> % Weekday
            total_swim_distance((Day rem 7) + 1, RemainingDays - 1, Acc + 250)
    end.

test() ->
    2000 = total_swim_distance(3, 10),
    1250 = total_swim_distance(6, 7),
    2500 = total_swim_distance(1, 14),
    500 = total_swim_distance(7, 3),
    ok.