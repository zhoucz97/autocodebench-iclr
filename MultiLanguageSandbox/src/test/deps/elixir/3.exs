defmodule SortedListMerger do
def merge_sorted_lists(list1, list2) do
    merge(list1, list2, [])
    |> Enum.reverse()
  end

  defp merge([], [], acc), do: acc
  defp merge([], list2, acc), do: Enum.reverse(acc) ++ list2
  defp merge(list1, [], acc), do: Enum.reverse(acc) ++ list1
  defp merge([h1 | t1], [h2 | t2], acc) when h1 <= h2 do
    merge(t1, [h2 | t2], [h1 | acc])
  end
  defp merge([h1 | t1], [h2 | t2], acc) do
    merge([h1 | t1], t2, [h2 | acc])
  end
end



ExUnit.start()
defmodule SortedListMergerTest do
use ExUnit.Case

test "merges two non-empty sorted lists" do
assert SortedListMerger.merge_sorted_lists([1, 3, 5], [2, 4, 6]) == [1, 2, 3, 4, 5, 6]
assert SortedListMerger.merge_sorted_lists([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
end

test "merges a non-empty list with an empty list" do
assert SortedListMerger.merge_sorted_lists([], [1, 2, 3]) == [1, 2, 3]
assert SortedListMerger.merge_sorted_lists([1, 2, 3], []) == [1, 2, 3]
end

test "merges two empty lists" do
assert SortedListMerger.merge_sorted_lists([], []) == []
end
end