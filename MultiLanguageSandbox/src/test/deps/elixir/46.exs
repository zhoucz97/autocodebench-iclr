defmodule ArithmeticSequence do
def nth_term(a1, a2, n) when is_integer(a1) and is_integer(a2) and is_integer(n) do
    # Calculate the common difference
    d = a2 - a1
    
    # Calculate the nth term using the formula: a_n = a1 + (n - 1) * d
    a1 + (n - 1) * d
  end
end



ExUnit.start()
defmodule ArithmeticSequenceTest do
  use ExUnit.Case

  test "nth_term calculates correctly for positive difference" do
    assert ArithmeticSequence.nth_term(1, 4, 100) == 298
    assert ArithmeticSequence.nth_term(0, 5, 20) == 95
  end

  test "nth_term calculates correctly for negative difference" do
    assert ArithmeticSequence.nth_term(10, 5, 3) == 0
    assert ArithmeticSequence.nth_term(20, 15, 4) == 5
  end

  test "nth_term handles sequences with zero as a term" do
    assert ArithmeticSequence.nth_term(-3, 3, 3) == 9
    assert ArithmeticSequence.nth_term(-10, -5, 5) == 10
  end

  test "nth_term calculates correctly for single-digit increments" do
    assert ArithmeticSequence.nth_term(1, 2, 100) == 100
    assert ArithmeticSequence.nth_term(-1, 0, 50) == 48
  end
end