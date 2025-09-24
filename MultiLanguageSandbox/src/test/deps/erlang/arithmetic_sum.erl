-module(arithmetic_sum).
-export([sum_of_n_terms/3, test/0]).

sum_of_n_terms(A1, A2, N) ->
    D = A2 - A1,  % Calculate the common difference
    N * (2 * A1 + (N - 1) * D) div 2.  % Apply the arithmetic sequence sum formula

test() ->
    25 = sum_of_n_terms(1, 3, 5),
    21 = sum_of_n_terms(5, 7, 3),
    10 = sum_of_n_terms(-2, 1, 4),
    30 = sum_of_n_terms(10, 20, 2),
    5050 = sum_of_n_terms(1, 2, 100), % Sum of the first 100 natural numbers
    ok.