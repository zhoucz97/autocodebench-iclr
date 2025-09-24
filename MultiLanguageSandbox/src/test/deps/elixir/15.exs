defmodule QuickSort do
def quicksort([]), do: []
  def quicksort([pivot | rest]) do
    {less, greater} = Enum.split_with(rest, &(&1 <= pivot))
    quicksort(less) ++ [pivot] ++ quicksort(greater)
  end
end



ExUnit.start()
defmodule QuickSortTest do
use ExUnit.Case

test "quicksort algorithm" do
assert QuickSort.quicksort([3, 2, 5, 1, 4]) == [1, 2, 3, 4, 5]
assert QuickSort.quicksort([10, 7, 8, 9, 1, 5]) == [1, 5, 7, 8, 9, 10]
end
end