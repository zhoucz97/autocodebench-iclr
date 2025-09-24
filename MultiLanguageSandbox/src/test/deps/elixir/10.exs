defmodule ProductCalculator do
def calculate_product_except_self(nums) do
    # Calculate the total product of all numbers
    total_product = Enum.reduce(nums, 1, &(&1 * &2))
    
    # For each number, divide the total product by the current number
    Enum.map(nums, fn num -> div(total_product, num) end)
  end
end



ExUnit.start()
defmodule ProductCalculatorTest do
use ExUnit.Case

test "calculates product of all elements except self" do
assert ProductCalculator.calculate_product_except_self([1, 2, 3, 4]) == [24, 12, 8, 6]
assert ProductCalculator.calculate_product_except_self([0, 1]) == [1, 0]
assert ProductCalculator.calculate_product_except_self([0, 0, 1]) == [0, 0, 0]
end
end
