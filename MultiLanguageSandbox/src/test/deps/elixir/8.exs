defmodule StockProfitCalculator do
def max_profit(prices) do
    prices
    |> Enum.reduce({0, nil}, fn price, {max_profit, min_price} ->
      min_price = if min_price == nil or price < min_price, do: price, else: min_price
      current_profit = price - min_price
      max_profit = if current_profit > max_profit, do: current_profit, else: max_profit
      {max_profit, min_price}
    end)
    |> elem(0)
  end
end



ExUnit.start()
defmodule StockProfitCalculatorTest do
use ExUnit.Case

test "calculates maximum profit" do
assert StockProfitCalculator.max_profit([7, 1, 5, 3, 6, 4]) == 5
assert StockProfitCalculator.max_profit([7, 6, 4, 3, 1]) == 0
end

test "handles empty array" do
assert StockProfitCalculator.max_profit([]) == 0
end

test "handles single element array" do
assert StockProfitCalculator.max_profit([5]) == 0
end
end