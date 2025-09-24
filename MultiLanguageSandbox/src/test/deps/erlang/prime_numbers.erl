-module(prime_numbers).
-export([get_prime/1, test/0]).

get_prime(N) ->
    lists:foldl(fun(X, Acc) -> 
                    case is_prime(X) of
                        true -> Acc + 1;
                        false -> Acc
                    end
                end, 0, lists:seq(1, N - 1)).

% Test function to verify the examples
test() ->
0 = get_prime(2),
1 = get_prime(3),
4 = get_prime(10),
8 = get_prime(20),
ok.