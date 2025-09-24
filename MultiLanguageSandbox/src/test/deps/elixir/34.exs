defmodule PencilPurchaseCalculator do
def minimum_cost(n, packages) do
    max_pencils = Enum.max_by(packages, fn {p, _} -> p end) |> elem(0)
    dp_size = n + max_pencils
    dp = List.duplicate(:infinity, dp_size + 1)
    dp = List.replace_at(dp, 0, 0)
    
    dp = Enum.reduce(packages, dp, fn {pencils, price}, acc ->
      Enum.reduce(0..dp_size, acc, fn i, acc_inner ->
        if i >= pencils and acc_inner.[i - pencils] != :infinity do
          new_cost = acc_inner.[i - pencils] + price
          if new_cost < acc_inner.[i] do
            List.replace_at(acc_inner, i, new_cost)
          else
            acc_inner
          end
        else
          acc_inner
        end
      end)
    end)
    
    min_cost = Enum.min(Enum.slice(dp, n..dp_size))
    if min_cost == :infinity, do: 0, else: min_cost
  end
end



ExUnit.start()
defmodule PencilPurchaseCalculatorTest do
use ExUnit.Case

test "calculates minimum cost for required pencils" do
assert PencilPurchaseCalculator.minimum_cost(57, [{2, 2}, {50, 30}, {30, 27}]) == 54
assert PencilPurchaseCalculator.minimum_cost(100, [{10, 5}, {20, 7}, {30, 10}]) == 35
end

test "handles single package type" do
assert PencilPurchaseCalculator.minimum_cost(50, [{10, 10}]) == 50
end

test "handles exact quantity match" do
assert PencilPurchaseCalculator.minimum_cost(60, [{30, 15}, {60, 25}, {90, 35}]) == 25
end
end