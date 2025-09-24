-module(split_string).
-export([split/2, test/0]).

split(String, Delimiter) ->
    string:split(String, Delimiter, all).
test() ->
["one", "two", "three"] = split("one,two,three", ","),
["hello", "world"] = split("hello world", " "),
["one", "two", "three"] = split("one two three", " "),
["", ""] = split(",", ","),
["hello"] = split("hello", ","),
ok.