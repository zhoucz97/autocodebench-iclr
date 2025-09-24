-module(median_without_extremes).
-export([median/1, test/0]).

median(List) ->
    Sorted = lists:sort(List),
    case length(Sorted) of
        0 -> undefined;  % Handle empty list case
        1 -> undefined;  % Can't remove min/max from single-element list
        _ ->
            WithoutExtremes = tl(tl(reverse(tl(reverse(Sorted))))),  % Remove min and max
            calculate_median(WithoutExtremes)
    end.

calculate_median(List) ->
    Length = length(List),
    case Length rem 2 of
        1 ->  % Odd length
            MiddleIndex = (Length div 2) + 1,
            lists:nth(MiddleIndex, List);
        0 ->  % Even length
            Middle1Index = Length div 2,
            Middle2Index = Middle1Index + 1,
            (lists:nth(Middle1Index, List) + lists:nth(Middle2Index, List)) / 2
    end.

test() ->
    6 = median([10, 5, 1, 8, 7, 6, 2]),
    3 = median([1, 2, 3, 4, 5]),
    3.5 = median([10, 10, 5, 2, 2, 1]),
    2 = median([1, 2, 3]),
    4.5 = median([1, 2, 3, 4, 5, 6, 7, 8]),
    ok.