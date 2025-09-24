defmodule YearChecker do
def leap_year?(year) when is_integer(year) do
    (rem(year, 4) == 0 and rem(year, 100) != 0) or (rem(year, 400) == 0)
  end
end



ExUnit.start()
defmodule YearCheckerTest do
  use ExUnit.Case

  test "year divisible by 4 but not 100 is leap year" do
    assert YearChecker.leap_year?(2024) == true
  end

  test "year divisible by 100 but not 400 is not leap year" do
    assert YearChecker.leap_year?(1900) == false
  end

  test "year divisible by 400 is leap year" do
    assert YearChecker.leap_year?(2000) == true
  end

  test "year not divisible by 4 is not leap year" do
    assert YearChecker.leap_year?(2019) == false
  end
end