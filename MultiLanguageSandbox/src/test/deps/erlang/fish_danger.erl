-module(fish_danger).
-export([is_danger/2, test/0]).

is_danger(S, X) ->
    % Initial position and speed
    Position = 0,
    Speed = 7,
    % Call the helper function to simulate the fish's movement
    simulate(Position, Speed, S, X).

% Helper function to simulate the fish's movement
simulate(Position, Speed, S, X) ->
    % Calculate the new position after one second
    NewPosition = Position + Speed,
    % Check if the fish is within the detection range
    if
        % If the fish is within the detection range
        (NewPosition >= S - X) and (NewPosition =< S + X) ->
            % Calculate the position after the next second
            NextSpeed = Speed * 0.98,
            NextPosition = NewPosition + NextSpeed,
            % Check if the fish is still within the detection range after the next second
            if
                (NextPosition >= S - X) and (NextPosition =< S + X) ->
                    % The fish is in danger
                    'y';
                true ->
                    % The fish is not in danger
                    'n'
            end;
        true ->
            % The fish is not within the detection range, continue simulation
            simulate(NewPosition, Speed * 0.98, S, X)
    end.

% Test function to verify the implementation
test() ->
    'n' = is_danger(14, 1),
    'n' = is_danger(20, 2),
    'n' = is_danger(10, 1),
    'n' = is_danger(14, 0.5),
    'n' = is_danger(30, 3),
    ok.