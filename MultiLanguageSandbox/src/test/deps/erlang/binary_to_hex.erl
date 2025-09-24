-module(binary_to_hex).
-export([binary_to_hex/1, test/0]).

binary_to_hex(<<Byte:8, Rest/binary>>, Acc) ->
    Hex = io_lib:format("~2.16.0B", [Byte]),
    binary_to_hex(Rest, Hex ++ Acc).
test() ->
"DEADBEEF" = binary_to_hex(<<16#DEADBEEF:32>>),
"ABCDEF" = binary_to_hex(<<16#ABCDEF:24>>),
"00" = binary_to_hex(<<0:8>>),
"FF" = binary_to_hex(<<255:8>>),
"" = binary_to_hex(<<>>),
ok.