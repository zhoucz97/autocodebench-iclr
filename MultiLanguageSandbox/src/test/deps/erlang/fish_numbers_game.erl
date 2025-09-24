-module(fish_numbers_game).
-export([reverse_sequence/1, test/0]).

reverse_sequence(Sequence) ->
    % Find the index of the first 0 in the sequence
    case lists:member(0, Sequence) of
        true ->
            {BeforeZero, _} = lists:splitwith(fun(X) -> X =/= 0 end, Sequence),
            lists:reverse(BeforeZero);
        false ->
            lists:reverse(Sequence)
    end.

% Alternatively, a more concise version:
% reverse_sequence(Sequence) ->
%     lists:reverse(lists:dropwhile(fun(0) -> false; (_) -> true end, Sequence)).

test() ->
    [30, 1, 34, 5, 23, 65, 3] = reverse_sequence([3, 65, 23, 5, 34, 1, 30, 0]),
    [10] = reverse_sequence([10, 0]),
    [3, 2, 1] = reverse_sequence([1, 2, 3, 0]),
    [] = reverse_sequence([0]),
    [2, 1] = reverse_sequence([1, 2, 0]),
    ok.