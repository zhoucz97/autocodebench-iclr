-module(sum_values_by_key).
-export([sum_values_and_sort_keys/1, test/0]).

sum_values_and_sort_keys(List) ->
    % Step 1: Sum the values for each key
    Sums = lists:foldl(fun({Key, Value}, Acc) ->
        case maps:find(Key, Acc) of
            {ok, CurrentValue} -> maps:put(Key, CurrentValue + Value, Acc);
            error -> maps:put(Key, Value, Acc)
        end
    end, #{}, List),
    
    % Step 2: Convert the map back to a list of tuples
    MappedList = maps:to_list(Sums),
    
    % Step 3: Sort the list by key
    SortedList = lists:sort(fun({K1, _}, {K2, _}) -> K1 =< K2 end, MappedList),
    
    SortedList.
test() ->
[{a, 4}, {b, 2}] = sum_values_and_sort_keys([{a, 1}, {b, 2}, {a, 3}]),
[{bar, 20}, {baz, 40}, {foo, 40}] = sum_values_and_sort_keys([{foo, 10}, {bar, 20}, {foo, 30}, {baz, 40}]),
[] = sum_values_and_sort_keys([]),
[{x, 1}] = sum_values_and_sort_keys([{x, 1}]),
ok.