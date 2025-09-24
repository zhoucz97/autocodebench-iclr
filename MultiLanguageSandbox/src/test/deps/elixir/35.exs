defmodule PasswordGenerator do
def generate_password(n, original_string) do
    original_string
    |> String.to_charlist()
    |> Enum.map(&shift_char(&1, n))
    |> List.to_string()
  end

  defp shift_char(char, n) when char in ?a..?z do
    rem(char - ?a + n, 26) + ?a
  end

  defp shift_char(char, _n), do: char
end



ExUnit.start()
defmodule PasswordGeneratorTest do
use ExUnit.Case

test "generates password with character shift" do
assert PasswordGenerator.generate_password(1, "abc") == "bcd"
assert PasswordGenerator.generate_password(2, "xyz") == "zab"
end

test "handles wrap around from z to a" do
assert PasswordGenerator.generate_password(1, "z") == "a"
end

test "handles large shifts" do
assert PasswordGenerator.generate_password(26, "abc") == "abc"
assert PasswordGenerator.generate_password(52, "xyz") == "xyz"
end
end