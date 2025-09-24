defmodule JinjinUnhappinessCalculator do
def calculate_unhappiness(n, daily_schedules) do
    Enum.reduce(daily_schedules, 0, fn {school_hours, extra_hours}, acc ->
      total = school_hours + extra_hours
      if total > 8 do
        acc + (total - 8)
      else
        acc
      end
    end)
  end
end



ExUnit.start()
defmodule JinjinUnhappinessCalculatorTest do
use ExUnit.Case

test "calculates cumulative unhappiness correctly" do
assert JinjinUnhappinessCalculator.calculate_unhappiness(7, [{5, 3}, {6, 2}, {7, 2}, {5, 3}, {5, 4}, {0, 4}, {0, 6}]) == -4
assert JinjinUnhappinessCalculator.calculate_unhappiness(3, [{5, 4}, {6, 3}, {7, 2}]) == 3
end

test "handles no unhappiness case" do
assert JinjinUnhappinessCalculator.calculate_unhappiness(2, [{8, 0}, {7, 1}]) == 0
end

test "handles single day case" do
assert JinjinUnhappinessCalculator.calculate_unhappiness(1, [{9, 0}]) == 1
end
end