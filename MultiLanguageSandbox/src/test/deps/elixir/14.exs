defmodule DuplicateChecker do
def has_duplicates(nums) do
    nums != Enum.uniq(nums)
  end
end



ExUnit.start()
defmodule DuplicateCheckerTest do
use ExUnit.Case

test "check for duplicates" do
refute DuplicateChecker.has_duplicates([1, 2, 3, 4, 5])
assert DuplicateChecker.has_duplicates([1, 2, 3, 3, 4, 5])
end

test "check for no duplicates" do
refute DuplicateChecker.has_duplicates([10, 20, 30, 40, 50])
assert DuplicateChecker.has_duplicates([10, 20, 30, 20, 40, 50])
end
end