-module(two_sum).
-export([find_two_sum/2, test/0]).

find_two_sum([Num | Rest], Target, Index, Map) ->
    Complement = Target - Num,
    case maps:is_key(Complement, Map) of
        true ->
            {maps:get(Complement, Map), Index};
        false ->
            NewMap = maps:put(Num, Index, Map),
            find_two_sum(Rest, Target, Index + 1, NewMap)
    end.
test() ->
{0, 1} = find_two_sum([2, 7, 11, 15], 9),
{1, 2} = find_two_sum([3, 2, 4], 6),
not_found = find_two_sum([1, 2, 3], 7),
ok.