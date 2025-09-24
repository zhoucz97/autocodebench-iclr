-module(leap_year_sum).
-export([sum_leap_years/2, test/0]).

sum_leap_years(StartYear, EndYear) ->
    Years = lists:seq(StartYear + 1, EndYear - 1),
    LeapYears = lists:filter(fun is_leap_year/1, Years),
    lists:sum(LeapYears).

% Test function
test() ->
8040 = sum_leap_years(2000, 2020),
2020 = sum_leap_years(2018, 2022),
0= sum_leap_years(2021, 2022),
0 = sum_leap_years(2001, 2003),
ok.