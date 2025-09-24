defmodule AsciiConverter do
def ascii_to_char(ascii_code) when is_integer(ascii_code) and ascii_code > 0 and ascii_code < 128 do
    <<ascii_code::utf8>>
  end
end



ExUnit.start()
defmodule AsciiConverterTest do
  use ExUnit.Case

  test "ascii_to_char returns the correct character for uppercase letters" do
    assert AsciiConverter.ascii_to_char(65) == "A"
    assert AsciiConverter.ascii_to_char(90) == "Z"
  end

  test "ascii_to_char returns the correct character for lowercase letters" do
    assert AsciiConverter.ascii_to_char(97) == "a"
    assert AsciiConverter.ascii_to_char(122) == "z"
  end

  test "ascii_to_char returns the correct character for digits" do
    assert AsciiConverter.ascii_to_char(48) == "0"
    assert AsciiConverter.ascii_to_char(57) == "9"
  end

  test "ascii_to_char returns the correct character for special characters" do
    assert AsciiConverter.ascii_to_char(33) == "!"
    assert AsciiConverter.ascii_to_char(64) == "@"
  end
end