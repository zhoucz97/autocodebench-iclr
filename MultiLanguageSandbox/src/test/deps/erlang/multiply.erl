-module(multiply).
-export([product/2, test/0]).

product(A, B) when A >= 1, A =< 50000, B >= 1, B =< 50000 ->
    A * B.

% Test function to verify the correctness of the product function
test() ->
    12 = product(3, 4),
    50000 = product(1, 50000),
    56088 = product(123, 456),
    100000 = product(50000, 2),
    12321 = product(111, 111),
    ok.