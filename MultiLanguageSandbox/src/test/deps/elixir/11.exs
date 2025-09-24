defmodule TwoSum do
def find_indices(nums, target) do
    Enum.reduce_while(nums, {%{}, 0}, fn num, {complements, index} ->
      case Map.get(complements, num) do
        nil ->
          new_complements = Map.put(complements, target - num, index)
          {:cont, {new_complements, index + 1}}
        complement_index ->
          {:halt, {complement_index, index}}
      end
    end)
  end
end



ExUnit.start()
defmodule TwoSumTest do
use ExUnit.Case

test "finds indices of two numbers that add up to the target" do
assert TwoSum.find_indices([2, 7, 11, 15], 9) == {0, 1}
assert TwoSum.find_indices([3, 2, 4], 6) == {1, 2}
assert TwoSum.find_indices([3, 3], 6) == {0, 1}
end
end