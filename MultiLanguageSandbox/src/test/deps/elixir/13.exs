defmodule SingleNumberFinder do
def find_single_number(nums) do
    Enum.reduce(nums, 0, fn num, acc -> Bitwise.bxor(acc, num) end)
  end
end

# Example usage:


ExUnit.start()
defmodule SingleNumberFinderTest do
use ExUnit.Case

test "find single number" do
assert SingleNumberFinder.find_single_number([2, 2, 1]) == 1
assert SingleNumberFinder.find_single_number([4, 1, 2, 1, 2]) == 4
assert SingleNumberFinder.find_single_number([1]) == 1
end
end