defmodule NumberUtils do
def reverse_list(list) do
    Enum.reverse(list)
  end
end



ExUnit.start()
defmodule NumberUtilsTest do
use ExUnit.Case

test "reverse list" do
assert NumberUtils.reverse_list([1, 2, 3, 4]) == [4, 3, 2, 1]
assert NumberUtils.reverse_list(["a", "b", "c"]) == ["c", "b", "a"]
end
end