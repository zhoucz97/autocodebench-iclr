-module(extract_numbers).
-export([extract_numbers/1, test/0]).

extract_numbers([_ | Rest], CurrentNum, Acc) ->
    extract_numbers(Rest, [], [list_to_integer(lists:reverse(CurrentNum)) | Acc]).

test() ->
[123, 45, 6] = extract_numbers("abc123def45ghi6"),
[] = extract_numbers("no numbers"),
[2023] = extract_numbers("year2023"),
[1, 2, 3] = extract_numbers("1a2b3c"),
[] = extract_numbers(""),
ok.