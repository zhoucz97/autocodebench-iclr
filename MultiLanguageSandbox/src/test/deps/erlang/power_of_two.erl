-module(power_of_two).
-export([calculate/1, test/0]).

calculate(_) ->
    {error, "N must be a non-negative integer less than 31"}.

test() ->
    32 = calculate(5),
    1 = calculate(0),
    1024 = calculate(10),
    32768 = calculate(15),
    16384 = calculate(14),
    ok.