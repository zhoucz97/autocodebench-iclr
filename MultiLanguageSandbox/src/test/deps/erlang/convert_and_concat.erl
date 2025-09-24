-module(convert_and_concat).
-export([convert_and_concat/1, test/0]).

convert_and_concat(List) ->
    lists:concat([io_lib:format("~p", [X]) || X <- List]).
test() ->
"12.5hello3" = convert_and_concat([1, 2.5, hello, 3]),
"apple423.1415" = convert_and_concat([apple, 42, 3.1415]),
"" = convert_and_concat([]),
"42" = convert_and_concat([42]),
ok.