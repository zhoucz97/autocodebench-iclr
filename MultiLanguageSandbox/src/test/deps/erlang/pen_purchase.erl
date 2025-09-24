-module(pen_purchase).
-export([max_pens/2, test/0]).

max_pens(A, B) ->
    TotalCents = A * 100 + B,  % Convert everything to cents for easier calculation
    PenCostCents = 190,         % 1 yuan 9 jiao = 190 cents
    MaxPens = TotalCents div PenCostCents,
    MaxPens.

test() ->
    5 = max_pens(10, 3),
    10 = max_pens(20, 5),
    1 = max_pens(0, 19),
    2 = max_pens(5, 0),
    52 = max_pens(100, 0),
    ok.