defmodule ArrayUtils do
def remove_value(nums, val) do
    filtered = Enum.reject(nums, &(&1 == val))
    length(filtered)
  end
end



ExUnit.start()
defmodule ArrayUtilsTest do
use ExUnit.Case

test "removes value and returns new length" do
assert ArrayUtils.remove_value([3, 2, 2, 3], 3) == 2
assert ArrayUtils.remove_value([0, 1, 2, 2, 3, 0, 4, 2], 2) == 5
end

test "returns same length when value not present" do
assert ArrayUtils.remove_value([1, 2, 3, 4, 5], 6) == 5
end

test "returns zero when all elements are the value" do
assert ArrayUtils.remove_value([2, 2, 2, 2], 2) == 0
end

test "handles empty list" do
assert ArrayUtils.remove_value([], 1) == 0
end
end