-module(swimming_time).
-export([calculate_time/4, test/0]).

calculate_time(A, B, C, D) ->
    StartMinutes = A * 60 + B,
    EndMinutes = C * 60 + D,
    TotalMinutes = if
        EndMinutes >= StartMinutes -> EndMinutes - StartMinutes;
        true -> (24 * 60 - StartMinutes) + EndMinutes
    end,
    Hours = TotalMinutes div 60,
    Minutes = TotalMinutes rem 60,
    {Hours, Minutes}.

test() ->
    {6, 20} = calculate_time(12, 50, 19, 10),
    {1, 15} = calculate_time(9, 30, 10, 45),
    {8, 20} = calculate_time(14, 55, 23, 15),
    {0, 45} = calculate_time(0, 15, 1, 0),
    {23, 45} = calculate_time(0, 0, 23, 45),
    ok.