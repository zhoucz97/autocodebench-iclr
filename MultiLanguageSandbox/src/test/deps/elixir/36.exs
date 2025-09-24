defmodule PalindromeUtils do
def count_palindromes(list) do
    list
    |> Enum.count(&is_palindrome?/1)
  end

  defp is_palindrome?(number) do
    str = Integer.to_string(number)
    str == String.reverse(str)
  end
end



ExUnit.start()

defmodule PalindromeUtilsTest do
  use ExUnit.Case

  test "counts palindromes with mixed numbers" do
    assert PalindromeUtils.count_palindromes([121, 131, 20]) == 2
  end

  test "counts no palindromes in list" do
    assert PalindromeUtils.count_palindromes([123, 324, 345]) == 0
  end

  test "counts all numbers as palindromes" do
    assert PalindromeUtils.count_palindromes([11, 22, 33, 44, 55]) == 5
  end

  test "handles an empty list" do
    assert PalindromeUtils.count_palindromes([]) == 0
  end

  test "handles a list with single-digit numbers" do
    assert PalindromeUtils.count_palindromes([1, 2, 3, 4, 5, 6, 7, 8, 9]) == 9
  end
end