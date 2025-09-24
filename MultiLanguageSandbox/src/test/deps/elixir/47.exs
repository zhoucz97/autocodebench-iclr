defmodule Multiplication do
def multiply(a, b) when is_integer(a) and is_integer(b) and a >= 1 and b >= 1 do
    a * b
  end
end



ExUnit.start()
defmodule MultiplicationTest do
  use ExUnit.Case

  test "multiply returns the correct product for small numbers" do
    assert Multiplication.multiply(3, 4) == 12
    assert Multiplication.multiply(7, 5) == 35
  end

  test "multiply returns the correct product for larger numbers" do
    assert Multiplication.multiply(123, 456) == 56088
    assert Multiplication.multiply(50000, 2) == 100000
  end

  test "multiply handles edge cases" do
    assert Multiplication.multiply(1, 50000) == 50000
    assert Multiplication.multiply(50000, 1) == 50000
  end
end