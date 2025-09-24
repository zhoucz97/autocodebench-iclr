defmodule ArrayRotator do
def rotate_right(nums, k) do
    len = length(nums)
    k = rem(k, len)  # Handle cases where k is larger than the list length
    split_point = len - k
    {left, right} = Enum.split(nums, split_point)
    right ++ left
  end
end



ExUnit.start()
defmodule ArrayRotatorTest do
use ExUnit.Case

test "rotates array to the right by k positions" do
assert ArrayRotator.rotate_right([1, 2, 3, 4, 5, 6, 7], 3) == [5, 6, 7, 1, 2, 3, 4]
assert ArrayRotator.rotate_right([-1, -100, 3, 99], 2) == [3, 99, -1, -100]
end

test "handles rotation larger than array length" do
assert ArrayRotator.rotate_right([1, 2, 3], 4) == [3, 1, 2]
end

test "handles zero rotation" do
assert ArrayRotator.rotate_right([1, 2, 3], 0) == [1, 2, 3]
end

test "handles empty array" do
assert ArrayRotator.rotate_right([], 3) == []
end
end