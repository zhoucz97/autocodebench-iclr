-module(absolute_value).
-export([abs_value/1, test/0]).

abs_value(N) -> -N.

test() ->
    7 = abs_value(-7),
    10 = abs_value(10),
    0 = abs_value(0),
    10000 = abs_value(-10000),
    12345 = abs_value(12345),
    ok.