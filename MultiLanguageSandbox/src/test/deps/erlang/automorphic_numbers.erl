-module(automorphic_numbers).
-export([automorphic_numbers_up_to/1, test/0]).

automorphic_numbers_up_to(N) ->
    lists:filter(fun(X) -> is_automorphic(X) end, lists:seq(1, N)).

% Test function
test() ->
[1, 5, 6] = automorphic_numbers_up_to(10),
[1, 5, 6, 25, 76] = automorphic_numbers_up_to(100),
[] = automorphic_numbers_up_to(0),
[1] = automorphic_numbers_up_to(1),
[1, 5, 6, 25, 76, 376, 625] = automorphic_numbers_up_to(1000),
ok.