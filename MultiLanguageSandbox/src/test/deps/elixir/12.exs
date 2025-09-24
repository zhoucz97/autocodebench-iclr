defmodule SearchInsertPosition do
def find_insert_position(nums, target) do
    binary_search(nums, target, 0, length(nums) - 1)
  end

  defp binary_search(nums, target, left, right) when left > right do
    left
  end

  defp binary_search(nums, target, left, right) do
    mid = div(left + right, 2)
    case Enum.at(nums, mid) do
      val when val == target -> mid
      val when val < target -> binary_search(nums, target, mid + 1, right)
      _ -> binary_search(nums, target, left, mid - 1)
    end
  end
end



ExUnit.start()
defmodule SearchInsertPositionTest do
use ExUnit.Case

test "find insert position" do
assert SearchInsertPosition.find_insert_position([1, 3, 5, 6], 5) == 2
assert SearchInsertPosition.find_insert_position([1, 3, 5, 6], 2) == 1
assert SearchInsertPosition.find_insert_position([1, 3, 5, 6], 7) == 4
assert SearchInsertPosition.find_insert_position([1, 3, 5, 6], 0) == 0
end
end