-module(char_to_ascii).
-export([convert_to_ascii/1, test/0]).

convert_to_ascii(_) ->
    error(badarg).

test() ->
    65 = convert_to_ascii($A),
    97 = convert_to_ascii($a),
    48 = convert_to_ascii($0),
    36 = convert_to_ascii($$),
    33 = convert_to_ascii($!),
    ok.