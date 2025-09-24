defmodule AbsoluteValue do
def abs_value(n) when is_integer(n) do
    if n < 0 do
      -n
    else
      n
    end
  end
end



ExUnit.start()
defmodule AbsoluteValueTest do
  use ExUnit.Case

  test "abs_value returns the absolute value for negative numbers" do
    assert AbsoluteValue.abs_value(-1) == 1
    assert AbsoluteValue.abs_value(-10000) == 10000
  end

  test "abs_value returns the same value for positive numbers" do
    assert AbsoluteValue.abs_value(10) == 10
    assert AbsoluteValue.abs_value(9999) == 9999
  end

  test "abs_value returns 0 for 0" do
    assert AbsoluteValue.abs_value(0) == 0
  end
end