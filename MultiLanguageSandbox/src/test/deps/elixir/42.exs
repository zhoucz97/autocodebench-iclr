defmodule AsciiCode do
def get_ascii_code(char) when is_binary(char) and byte_size(char) == 1 do
    # Check if the character is a space (ASCII 32)
    if char == " " do
      raise ArgumentError, "Character cannot be a space"
    else
      # Get the ASCII code by converting the binary to a list of integers
      [code] = :binary.bin_to_list(char)
      code
    end
  end
end



ExUnit.start()
defmodule AsciiCodeTest do
  use ExUnit.Case

  test "get_ascii_code returns correct ASCII code for uppercase letters" do
    assert AsciiCode.get_ascii_code("A") == 65
    assert AsciiCode.get_ascii_code("Z") == 90
  end

  test "get_ascii_code returns correct ASCII code for lowercase letters" do
    assert AsciiCode.get_ascii_code("a") == 97
    assert AsciiCode.get_ascii_code("z") == 122
  end

  test "get_ascii_code returns correct ASCII code for digits" do
    assert AsciiCode.get_ascii_code("0") == 48
    assert AsciiCode.get_ascii_code("9") == 57
  end

  test "get_ascii_code returns correct ASCII code for special characters" do
    assert AsciiCode.get_ascii_code("!") == 33
    assert AsciiCode.get_ascii_code("@") == 64
  end
end