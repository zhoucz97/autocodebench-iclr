defmodule SnCalculator do
def find_min_n(k) when is_integer(k) do
    find_min_n(k, 1, 0.0)
  end

  defp find_min_n(k, n, sum) when sum > k, do: n - 1
  defp find_min_n(k, n, sum) do
    new_sum = sum + 1 / n
    find_min_n(k, n + 1, new_sum)
  end
end



ExUnit.start()
defmodule SnCalculatorTest do
  use ExUnit.Case

  test "finds minimum n for k=2" do
    assert SnCalculator.find_min_n(2) == 4
  end

  test "finds minimum n for k=3" do
    assert SnCalculator.find_min_n(3) == 11
  end

  test "finds minimum n for small k=1" do
    assert SnCalculator.find_min_n(1) == 2
  end

  test "finds minimum n for larger k=10" do
    assert SnCalculator.find_min_n(10) == 12367
  end
end