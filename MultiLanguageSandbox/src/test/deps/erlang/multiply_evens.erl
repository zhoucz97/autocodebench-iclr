-module(multiply_evens).
-export([multiply_evens/1, test/0]).

multiply_evens([_|T], Acc) ->
    multiply_evens(T, Acc).
test() ->
8 = multiply_evens([1, 2, 3, 4, 5]),
420 = multiply_evens([10, 23, 35, 42, 55]),
1 = multiply_evens([1, 3, 5, 7]),
2 = multiply_evens([2]),
1 = multiply_evens([]),
ok.