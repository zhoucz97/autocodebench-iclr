defmodule SignChecker do
def determine_sign(n) when is_integer(n) do
    cond do
      n > 0 -> "positive"
      n == 0 -> "zero"
      true -> "negative"
    end
  end
end



ExUnit.start()
defmodule SignCheckerTest do
  use ExUnit.Case

  test "determine_sign returns positive for positive integers" do
    assert SignChecker.determine_sign(1) == "positive"
    assert SignChecker.determine_sign(100) == "positive"
  end

  test "determine_sign returns zero for zero" do
    assert SignChecker.determine_sign(0) == "zero"
  end

  test "determine_sign returns negative for negative integers" do
    assert SignChecker.determine_sign(-1) == "negative"
    assert SignChecker.determine_sign(-100) == "negative"
  end
end