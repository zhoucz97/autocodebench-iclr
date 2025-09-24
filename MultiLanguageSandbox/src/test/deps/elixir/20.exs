defmodule FishCutenessUtils do
def count_less_cute_neighbors(cuteness_levels) do
    Enum.with_index(cuteness_levels)
    |> Enum.map(fn {current, index} ->
      left_fishes = Enum.take(cuteness_levels, index)
      Enum.count(left_fishes, &(&1 < current))
    end)
  end
end



ExUnit.start()
defmodule FishCutenessUtilsTest do
use ExUnit.Case

test "counts less cute neighbors correctly" do
assert FishCutenessUtils.count_less_cute_neighbors([4, 3, 0, 5, 1, 2]) == [0, 0, 0, 3, 1, 2]
assert FishCutenessUtils.count_less_cute_neighbors([5, 3, 4, 2, 1]) == [0, 0, 1, 0, 0]
end

test "handles equal cuteness levels" do
assert FishCutenessUtils.count_less_cute_neighbors([2, 2, 3, 3, 1]) == [0, 0, 2, 2, 0]
end

test "handles single fish" do
assert FishCutenessUtils.count_less_cute_neighbors([3]) == [0]
end
end