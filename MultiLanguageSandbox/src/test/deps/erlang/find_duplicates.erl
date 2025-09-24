-module(find_duplicates).
-export([sorted_duplicates_in_list/1, test/0]).

sorted_duplicates_in_list(List) ->
    % Count occurrences of each element
    Counts = lists:foldl(fun(Elem, Acc) ->
                            dict:update_counter(Elem, 1, Acc)
                         end, dict:new(), List),
    
    % Filter elements that appear more than once
    Duplicates = [Elem || {Elem, Count} <- dict:to_list(Counts), Count > 1],
    
    % Sort the duplicates in dictionary order
    lists:sort(Duplicates).
test() ->
[1, 2] = sorted_duplicates_in_list([1, 2, 3, 2, 4, 5, 1]),
["apple"] = sorted_duplicates_in_list(["banana", "apple", "apple", "cherry"]),
[] = sorted_duplicates_in_list([1, 2, 3, 4, 5]),
ok.