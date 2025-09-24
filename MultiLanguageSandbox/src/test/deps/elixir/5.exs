defmodule UniqueArrayUtils do
def remove_duplicates(nums) do
    nums
    |> Enum.uniq()
    |> length()
  end
end



ExUnit.start()
defmodule UniqueArrayUtilsTest do
use ExUnit.Case

test "removes duplicates and returns new length" do
assert UniqueArrayUtils.remove_duplicates([1, 1, 2]) == 2
assert UniqueArrayUtils.remove_duplicates([0, 0, 1, 1, 1, 2, 2, 3, 3, 4]) == 5
end

test "handles array with no duplicates" do
assert UniqueArrayUtils.remove_duplicates([1, 2, 3, 4, 5]) == 5
end

test "handles empty array" do
assert UniqueArrayUtils.remove_duplicates([]) == 0
end
end