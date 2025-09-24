defmodule NotebookTheftCalculator do
def calculate_theft(start_time, end_time, notebooks_per_second) do
    start_seconds = time_to_seconds(start_time)
    end_seconds = time_to_seconds(end_time)
    
    if end_seconds >= start_seconds do
      (end_seconds - start_seconds) * notebooks_per_second
    else
      # Handle cases where end_time is on the next day (assuming 24-hour wrap-around)
      (86400 - start_seconds + end_seconds) * notebooks_per_second
    end
  end
  
  defp time_to_seconds(time_str) do
    [hours, minutes, seconds] = 
      time_str
      |> String.split(":")
      |> Enum.map(&String.to_integer/1)
    
    hours * 3600 + minutes * 60 + seconds
  end
end



ExUnit.start()
defmodule NotebookTheftCalculatorTest do
use ExUnit.Case

test "calculates notebook theft correctly" do
assert NotebookTheftCalculator.calculate_theft("00:00:00", "00:00:10", 10) == 100
assert NotebookTheftCalculator.calculate_theft("01:23:45", "02:00:00", 5) == 10875
end

test "handles edge cases" do
assert NotebookTheftCalculator.calculate_theft("23:59:59", "00:00:01", 1) == 2
assert NotebookTheftCalculator.calculate_theft("12:30:00", "12:30:30", 2) == 60
end
end