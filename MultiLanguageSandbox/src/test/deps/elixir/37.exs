defmodule SearchUtils do
def binary_search(list, target) do
    binary_search_helper(list, target, 0, length(list) - 1)
  end

  defp binary_search_helper(_list, _target, low, high) when low > high, do: -1

  defp binary_search_helper(list, target, low, high) do
    mid = div(low + high, 2)
    mid_value = Enum.at(list, mid)

    cond do
      mid_value == target -> mid
      mid_value < target -> binary_search_helper(list, target, mid + 1, high)
      mid_value > target -> binary_search_helper(list, target, low, mid - 1)
    end
  end
end



ExUnit.start()

defmodule SearchUtilsTest do
  use ExUnit.Case

  test "finds the target in the middle of the list" do
    assert SearchUtils.binary_search([1, 2, 3, 4, 5], 3) == 2
  end

  test "returns -1 if the target is not in the list" do
    assert SearchUtils.binary_search([1, 3, 5, 7, 9], 8) == -1
  end

  test "finds the target at the beginning of the list" do
    assert SearchUtils.binary_search([10, 20, 30, 40, 50], 10) == 0
  end

  test "handles an empty list" do
    assert SearchUtils.binary_search([], 1) == -1
  end

  test "finds the target at the end of the list" do
    assert SearchUtils.binary_search([1, 2, 3, 4, 5], 5) == 4
  end
end