defmodule ReverseNumber do
def reverse_three_digit(number) when is_integer(number) and number >= 100 and number <= 999 do
    hundreds = div(number, 100)
    tens = rem(div(number, 10), 10)
    units = rem(number, 10)
    
    # Reverse the digits and convert back to integer
    reversed = units * 100 + tens * 10 + hundreds
    
    # Handle cases where the reversed number has leading zeros (like 001 becomes 1)
    if reversed == 0 do
      0
    else
      reversed
    end
  end
end



ExUnit.start()
defmodule ReverseNumberTest do
  use ExUnit.Case

  test "reverse_three_digit reverses numbers correctly" do
    assert ReverseNumber.reverse_three_digit(123) == 321
    assert ReverseNumber.reverse_three_digit(100) == 1  # Note: Leading zeros are not preserved in integers
    assert ReverseNumber.reverse_three_digit(905) == 509
    assert ReverseNumber.reverse_three_digit(210) == 12  # Note: Leading zeros result in a lower integer value
  end

  test "reverse_three_digit handles edge cases" do
    assert ReverseNumber.reverse_three_digit(999) == 999
    assert ReverseNumber.reverse_three_digit(101) == 101
  end
end