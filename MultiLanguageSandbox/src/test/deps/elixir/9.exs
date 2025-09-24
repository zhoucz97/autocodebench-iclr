defmodule JumpGame do
def can_reach_end?(nums) do
    can_reach_end?(nums, 0, 0)
  end

  defp can_reach_end?(_nums, current_index, max_reach) when current_index > max_reach, do: false
  defp can_reach_end?(_nums, current_index, _max_reach) when current_index == length(_nums) - 1, do: true
  defp can_reach_end?(nums, current_index, max_reach) do
    new_max_reach = max(max_reach, current_index + Enum.at(nums, current_index))
    can_reach_end?(nums, current_index + 1, new_max_reach)
  end
end



ExUnit.start()
defmodule JumpGameTest do
use ExUnit.Case

test "can reach the end" do
assert JumpGame.can_reach_end?([2, 3, 1, 1, 4])
refute JumpGame.can_reach_end?([3, 2, 1, 0, 4])
end

test "handles single element array" do
assert JumpGame.can_reach_end?([0])
end

test "handles empty array" do
refute JumpGame.can_reach_end?([])
end
end