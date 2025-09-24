defmodule ExperienceCalculator do
def calculate_level_and_exp(operations) do
    initial_life = 10
    initial_exp = 0
    initial_level = 0

    {final_level, final_exp, _} =
      Enum.reduce_while(operations, {initial_level, initial_exp, initial_life}, fn {life_change, exp_gain}, {level, exp, life} ->
        new_life = life + life_change
        if new_life <= 0 do
          {:halt, {level, exp, new_life}}
        else
          new_exp = exp + exp_gain
          {new_level, remaining_exp} = calculate_level_ups(level, new_exp)
          {:cont, {new_level, remaining_exp, new_life}}
        end
      end)

    {final_level, final_exp}
  end

  defp calculate_level_ups(level, exp) do
    required = :math.pow(2, level) |> round()
    if exp >= required do
      calculate_level_ups(level + 1, exp - required)
    else
      {level, exp}
    end
  end
end



ExUnit.start()
defmodule ExperienceCalculatorTest do
use ExUnit.Case

test "calculates final level and experience with valid operations" do
assert ExperienceCalculator.calculate_level_and_exp([{3, 5}, {-2, 8}]) == {3, 6}
end

test "handles operations resulting in death" do
assert ExperienceCalculator.calculate_level_and_exp([{10, 10}, {10, 20}]) == {0, 0}
end

test "handles operations with life recovery" do
assert ExperienceCalculator.calculate_level_and_exp([{-5, 5}, {-4, 10}, {2, 3}]) == {4, 3}
end
end