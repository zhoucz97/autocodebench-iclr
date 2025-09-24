defmodule ListUtils do
def concat_lists(list1, list2) do
    list1 ++ list2
  end
end



ExUnit.start()
defmodule ListUtilsTest do
use ExUnit.Case

test "concatenates non-empty lists" do
assert ListUtils.concat_lists([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
end

test "concatenates an empty list with a non-empty list" do
assert ListUtils.concat_lists([], [1, 2, 3]) == [1, 2, 3]
assert ListUtils.concat_lists([1, 2, 3], []) == [1, 2, 3]
end

test "concatenates two empty lists" do
assert ListUtils.concat_lists([], []) == []
end
end