defmodule WeatherAnalyzer do
def longest_rising_temperature_streak(temperatures) do
    temperatures
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.reduce({0, 0}, fn [prev, current], {current_streak, max_streak} ->
      if current > prev do
        new_streak = current_streak + 1
        {new_streak, max(new_streak, max_streak)}
      else
        {0, max_streak}
      end
    end)
    |> elem(1)
    |> Kernel.+(1)  # Add 1 because we're counting transitions between days
  end
end



ExUnit.start()
defmodule WeatherAnalyzerTest do
use ExUnit.Case

test "finds the longest rising temperature streak correctly" do
assert WeatherAnalyzer.longest_rising_temperature_streak([1, 2, 3, 2, 4, 5, 6, 8, 5, 9]) == 5
assert WeatherAnalyzer.longest_rising_temperature_streak([3, 4, 5, 1, 2]) == 3
end

test "handles constant temperatures" do
assert WeatherAnalyzer.longest_rising_temperature_streak([5, 5, 5, 5, 5]) == 1
end

test "handles single temperature" do
assert WeatherAnalyzer.longest_rising_temperature_streak([7]) == 1
end
end