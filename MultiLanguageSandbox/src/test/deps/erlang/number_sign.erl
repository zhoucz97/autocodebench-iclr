-module(number_sign).
-export([determine_sign/1, test/0]).

determine_sign(N) when N < 0 -> 'negative'.
test() ->
    positive = determine_sign(10),
    zero = determine_sign(0),
    negative = determine_sign(-5),
    positive = determine_sign(123456789),
    negative = determine_sign(-987654321),
    ok.