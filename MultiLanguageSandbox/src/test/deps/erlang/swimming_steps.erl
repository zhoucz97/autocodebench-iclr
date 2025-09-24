-module(swimming_steps).
-export([calculate_steps/1, test/0]).

calculate_steps(S, Steps, Total, CurrentStep) ->
    NewTotal = Total + CurrentStep,
    NewSteps = Steps + 1,
    NewCurrentStep = CurrentStep * 0.98,
    calculate_steps(S, NewSteps, NewTotal, NewCurrentStep).

test() ->
    3 = calculate_steps(4.3),
    1 = calculate_steps(2.0),
    4 = calculate_steps(6),
    ok.