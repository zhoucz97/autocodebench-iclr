defmodule MathOperation do
def calculate_expression(a, b, c) do
    (a + b) * c
  end
end



ExUnit.start()
defmodule MathOperationTest do
  use ExUnit.Case

  test "calculate_expression with positive numbers" do
    assert MathOperation.calculate_expression(2, 3, 4) == 20
  end

  test "calculate_expression with a negative number" do
    assert MathOperation.calculate_expression(-2, 3, 4) == 4
  end

  test "calculate_expression with zero" do
    assert MathOperation.calculate_expression(0, 0, 0) == 0
    assert MathOperation.calculate_expression(1, -1, 5) == 0
  end

  test "calculate_expression with mixed sign numbers" do
    assert MathOperation.calculate_expression(-5, 5, 3) == 0
    assert MathOperation.calculate_expression(10, -2, -3) == -24
  end
end