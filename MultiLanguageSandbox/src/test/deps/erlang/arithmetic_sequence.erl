-module(arithmetic_sequence).
-export([nth_term/3, test/0]).

nth_term(A1, A2, N) ->
    D = A2 - A1,  % Calculate the common difference
    A1 + (N - 1) * D.  % Calculate the nth term

test() ->
11 = nth_term(2, 5, 4),
24 = nth_term(-3, 0, 10),
0 = nth_term(0, 0, 100),
-9 = nth_term(-1, -3, 5),
100 = nth_term(1, 2, 100),
ok.