defmodule NumberGameUtils do
def reverse_numbers(nums) do
    nums
    |> Enum.reverse()
    |> Enum.drop_while(&(&1 == 0))
    |> Enum.reverse()
  end
end



ExUnit.start()
defmodule NumberGameUtilsTest do
use ExUnit.Case

test "reverses numbers and excludes trailing zero" do
assert NumberGameUtils.reverse_numbers([3, 65, 23, 5, 34, 1, 30, 0]) == [30, 1, 34, 5, 23, 65, 3]
assert NumberGameUtils.reverse_numbers([2, 5, 7, 0]) == [7, 5, 2]
end

test "handles single zero" do
assert NumberGameUtils.reverse_numbers([0]) == []
end

test "handles empty list" do
assert NumberGameUtils.reverse_numbers([]) == []
end
end