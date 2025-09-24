-module(expression_calc).
-export([calc/3, test/0]).

calc(A, B, C) ->
    (A + B) * C.

test() ->
    25 = calc(2, 3, 5),
    -9 = calc(-1, -2, 3),
    0 = calc(0, 0, 10),
    -600 = calc(100, 200, -2),
    0 = calc(5, 5, 0),
    ok.