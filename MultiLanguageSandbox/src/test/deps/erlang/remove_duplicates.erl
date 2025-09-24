-module(remove_duplicates).
-export([remove_duplicates_list/1, remove_duplicates_tuple/1, test/0]).

remove_duplicates_list([H|T], Acc, Set) ->
    case sets:is_element(H, Set) of
        true ->
            remove_duplicates_list(T, Acc, Set);
        false ->
            remove_duplicates_list(T, [H|Acc], sets:add_element(H, Set))
    end.
test() ->
[1,2,3, 4, 5] = remove_duplicates_list([1, 2, 3, 2, 4, 5, 1]),
["apple","banana", "cherry"] = remove_duplicates_list(["apple", "banana", "apple", "cherry"]),
[] = remove_duplicates_list([]),
{1,2,3, 4, 5} = remove_duplicates_tuple({1, 2, 3, 2, 4, 5, 1}),
{"apple","banana", "cherry"} = remove_duplicates_tuple({"apple", "banana", "apple", "cherry"}),
{} = remove_duplicates_tuple({}),
ok.