-module(int_bool_conversion).
-export([convert/1, test/0]).

convert(0) -> 0.

test() ->
    1 = convert(3),
    0 = convert(0),
    1 = convert(-5),
    1 = convert(100),
    1 = convert(2),
    ok.