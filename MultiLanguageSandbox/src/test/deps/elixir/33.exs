defmodule ElevatorTimeCalculator do
def total_time(floors) do
    # The elevator starts at floor 0, visits each floor in order, and then returns to 0.
    # So the path is 0 -> floor1 -> floor2 -> ... -> floorN -> 0.
    path = [0 | floors] ++ [0]
    
    {total_time, _} = Enum.reduce(Enum.chunk_every(path, 2, 1, :discard), {0, 0}, fn [from, to], {acc_time, _} ->
      delta = to - from
      move_time = if delta > 0, do: delta * 6, else: abs(delta) * 4
      new_acc_time = acc_time + move_time
      
      # Check if this 'to' floor is in the original floors list (meaning someone is getting off)
      stop_time = if Enum.member?(floors, to) do
        5 + Enum.count(floors, &(&1 == to))
      else
        0
      end
      
      {new_acc_time + stop_time, to}
    end)
    
    total_time
  end
end



ExUnit.start()
defmodule ElevatorTimeCalculatorTest do
use ExUnit.Case

test "calculates total elevator time for a given trip" do
assert ElevatorTimeCalculator.total_time([2, 3]) == 42
assert ElevatorTimeCalculator.total_time([1, 5, 2]) == 68
end

test "handles a single passenger" do
assert ElevatorTimeCalculator.total_time([3]) == 36
end

test "handles passengers to the same floor" do
assert ElevatorTimeCalculator.total_time([2, 2, 2]) == 28
end
end