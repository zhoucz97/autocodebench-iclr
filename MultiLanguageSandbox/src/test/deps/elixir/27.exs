defmodule KeypadTyping do
def count_key_presses(sentence) do
    sentence
    |> String.upcase()
    |> String.to_charlist()
    |> Enum.reduce(0, fn char, acc ->
      acc + Map.get(@key_presses, char, 0)
    end)
  end
end



ExUnit.start()

defmodule KeypadTypingTest do
use ExUnit.Case

test "counts key presses for simple sentences" do
assert KeypadTyping.count_key_presses("hello world") == 25
assert KeypadTyping.count_key_presses("good night") == 18
end

test "counts key presses for sentences with single letter words" do
assert KeypadTyping.count_key_presses("a b c") == 8
assert KeypadTyping.count_key_presses("i am here") == 16
end

test "handles sentences with multiple spaces" do
assert KeypadTyping.count_key_presses("abc def") == 13
end
end