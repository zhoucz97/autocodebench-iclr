-module(cow_conference).
-export([find_abc/1, test/0]).

find_abc(Numbers) ->
    Sorted = lists:sort(Numbers),
    S = lists:last(Sorted), % S is A + B + C, the largest number
    % The remaining six numbers include A, B, C, A+B, B+C, C+A
    Rest = lists:droplast(Sorted),
    % We need to find three numbers in Rest whose sum is S (since A + B + C = S)
    % But more accurately, the three smallest numbers could be A, B, C, but not necessarily.
    % Alternative approach: the sum of all seven numbers is 3S (A+B+C + (A+B)+(B+C)+(C+A) + (A+B+C) = 3(A+B+C))
    TotalSum = lists:sum(Numbers),
    ExpectedTotal = 3 * S,
    if
        TotalSum =/= ExpectedTotal ->
            error("Invalid input: total sum does not match 3S");
        true ->
            % Now, find A, B, C among the numbers where A + B + C = S
            % The approach is to pick the three smallest numbers as candidates for A, B, C
            % But need to verify that the other four numbers are A+B, B+C, C+A, and S
            Candidates = lists:sublist(Rest, 3),
            case verify_candidates(Candidates, S) of
                {ok, [A, B, C]} ->
                    lists:sort([A, B, C]);
                _ ->
                    % If the first three smallest don't work, try other combinations
                    % This is a brute-force approach for small lists
                    Combinations = combinations(Rest, 3),
                    case find_valid_combination(Combinations, S) of
                        {ok, [A, B, C]} ->
                            lists:sort([A, B, C]);
                        _ ->
                            error("No valid combination found")
                    end
            end
    end.

verify_candidates([A, B, C], S) ->
    Sum = A + B + C,
    if
        Sum =:= S ->
            Pairwise = [A+B, B+C, C+A, S],
            AllNumbers = [A, B, C | Pairwise],
            SortedAll = lists:sort(AllNumbers),
            case SortedAll =:= lists:sort([A, B, C, A+B, B+C, C+A, S]) of
                true ->
                    {ok, [A, B, C]};
                false ->
                    false
            end;
        true ->
            false
    end.

combinations(List, K) ->
    combinations(List, K, []).

combinations(_, 0, Acc) -> [Acc];
combinations([], _, _) -> [];
combinations([H|T], K, Acc) ->
    combinations(T, K - 1, [H|Acc]) ++ combinations(T, K, Acc).

find_valid_combination([], _) -> false;
find_valid_combination([Comb|Rest], S) ->
    case verify_candidates(Comb, S) of
        {ok, [A, B, C]} -> {ok, [A, B, C]};
        false -> find_valid_combination(Rest, S)
    end.

test() ->
[3, 5, 6] = find_abc([3, 6, 11, 9, 14, 5, 8]),
[10, 20, 30] = find_abc([10, 30, 20, 50, 40, 60, 70]),
[7, 14, 21] = find_abc([7, 28, 42, 35, 21, 49, 14]),
ok.