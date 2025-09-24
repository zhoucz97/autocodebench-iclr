-module(fibonacci).
-export([fib/1, test/0]).

fib(N, A, B) when N > 0 -> fib(N - 1, B, A + B).
test() ->
0 = fib(0),
1 = fib(1),
1 = fib(2),
2 = fib(3),
3 = fib(4),
5 = fib(5),
8 = fib(6),
13 = fib(7),
21 = fib(8),
34 = fib(9),
55 = fib(10),
ok.