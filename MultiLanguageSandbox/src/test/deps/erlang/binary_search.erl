-module(binary_search).
-export([search/2, test/0]).

search(List, Target, Low, High) ->
    Mid = (Low + High) div 2,
    MidValue = lists:nth(Mid, List),
    if
        MidValue =:= Target ->
            {found, Mid};
        MidValue < Target ->
            search(List, Target, Mid + 1, High);
        MidValue > Target ->
            search(List, Target, Low, Mid - 1)
    end.

test() ->
    {found, 3} = search([1, 2, 3, 4, 5], 3),
    not_found = search([1, 2, 4, 5, 6], 3),
    {found, 4} = search([10, 20, 30, 40, 50], 40),
    {found, 1} = search([100], 100),
    not_found = search([], 1),
    ok.