defmodule PowerOfTwo do
def power(n) when is_integer(n) and n >= 0 and n <= 30 do
    :math.pow(2, n) |> round()
  end
end



ExUnit.start()
defmodule PowerOfTwoTest do
  use ExUnit.Case

  test "power returns 1 for n = 0" do
    assert PowerOfTwo.power(0) == 1
  end

  test "power calculates 2^n for small n" do
    assert PowerOfTwo.power(1) == 2
    assert PowerOfTwo.power(2) == 4
    assert PowerOfTwo.power(3) == 8
  end

  test "power calculates 2^n for larger n" do
    assert PowerOfTwo.power(10) == 1024
    assert PowerOfTwo.power(20) == 1048576
  end

  test "power handles the maximum n value" do
    assert PowerOfTwo.power(30) == 1073741824
  end
end