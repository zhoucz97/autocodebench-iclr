defmodule GridShapeCalculator do
def count_shapes(n, m) do
    # Calculate the number of squares
    k = min(n, m)
    squares = Enum.reduce(1..k, 0, fn s, acc -> acc + (n - s + 1) * (m - s + 1) end)
    
    # Calculate the total number of rectangles (including squares)
    total_rectangles = div(n * (n + 1), 2) * div(m * (m + 1), 2)
    
    # Rectangles excluding squares
    rectangles = total_rectangles - squares
    
    {squares, rectangles}
  end
end



ExUnit.start()
defmodule GridShapeCalculatorTest do
use ExUnit.Case

test "counts squares and rectangles correctly" do
assert GridShapeCalculator.count_shapes(2, 3) == {8, 10}
assert GridShapeCalculator.count_shapes(3, 3) == {14, 22}
end

test "handles single row or column grid" do
assert GridShapeCalculator.count_shapes(1, 4) == {4, 6}
assert GridShapeCalculator.count_shapes(5, 1) == {5, 10}
end
end