defmodule StringSearchUtils do
def find_first_index(haystack, needle) do
    case String.split(haystack, needle, parts: 2) do
      [prefix, _] -> String.length(prefix)
      [_] -> -1
    end
  end
end



ExUnit.start()

defmodule StringSearchUtilsTest do
use ExUnit.Case

test "finds the first occurrence of needle in haystack" do
assert StringSearchUtils.find_first_index("hello", "ll") == 2
assert StringSearchUtils.find_first_index("abcabcabc", "abc") == 0
end

test "returns -1 if needle is not found in haystack" do
assert StringSearchUtils.find_first_index("abcde", "fgh") == -1
end

test "handles cases with multiple occurrences" do
assert StringSearchUtils.find_first_index("sadbutsad", "sad") == 0
end

test "handles empty needle" do
assert StringSearchUtils.find_first_index("hello", "") == 0
end
end