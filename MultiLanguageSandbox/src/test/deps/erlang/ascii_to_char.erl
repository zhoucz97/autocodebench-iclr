-module(ascii_to_char).
-export([convert_to_char/1, test/0]).

convert_to_char(AsciiCode) when is_integer(AsciiCode), AsciiCode >= 1, AsciiCode =< 127 ->
    [AsciiCode].

test() ->
    "A" = convert_to_char(65),
    "a" = convert_to_char(97),
    "0" = convert_to_char(48),
    "$" = convert_to_char(36),
    " " = convert_to_char(32),
    ok.