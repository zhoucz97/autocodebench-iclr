defmodule PenCalculator do
def max_pens(a, b) do
    total_jiao = a * 10 + b
    total_jiao div 19
  end
end



ExUnit.start()
defmodule PenCalculatorTest do
  use ExUnit.Case

  test "calculates maximum pens with exact amount" do
    assert PenCalculator.max_pens(3, 8) == 2
  end

  test "calculates maximum pens with extra jiao" do
    assert PenCalculator.max_pens(20, 0) == 10
  end

  test "calculates maximum pens with no extra jiao" do
    assert PenCalculator.max_pens(1, 9) == 1
  end

  test "calculates with zero yuan and some jiao" do
    assert PenCalculator.max_pens(0, 9) == 0
  end
end