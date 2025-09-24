-module(reverse_number).
-export([reverse/1, test/0]).

reverse(N) ->
    % Convert the number to a string to handle leading zeros
    Str = integer_to_list(N),
    % Pad with leading zeros if necessary to make it 3 digits
    PaddedStr = lists:sublist("000" ++ Str, -3),
    % Reverse the string
    ReversedStr = lists:reverse(PaddedStr),
    % Convert back to integer (this will remove leading zeros)
    list_to_integer(ReversedStr).

test() ->
    321 = reverse(123),
    1 = reverse(100), % Note: Erlang integers do not preserve leading zeros
    502 = reverse(205),
    98 = reverse(890), % Note: Erlang integers do not preserve leading zeros
    50 = reverse(050), % Note: Erlang integers do not preserve leading zeros, 050 is equivalent to 50 in Erlang
    ok.