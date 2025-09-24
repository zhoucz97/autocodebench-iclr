defmodule TypeConversion do
def int_to_bool_to_int(value) when is_integer(value) do
    # Convert integer to boolean (non-zero becomes true, zero becomes false)
    bool_value = value != 0
    
    # Convert boolean back to integer (true becomes 1, false becomes 0)
    if bool_value, do: 1, else: 0
  end
end



ExUnit.start()
defmodule TypeConversionTest do
  use ExUnit.Case

  test "int_to_bool_to_int returns 0 for input 0" do
    assert TypeConversion.int_to_bool_to_int(0) == 0
  end

  test "int_to_bool_to_int returns 1 for positive integers" do
    assert TypeConversion.int_to_bool_to_int(1) == 1
    assert TypeConversion.int_to_bool_to_int(10) == 1
  end

  test "int_to_bool_to_int returns 1 for negative integers" do
    assert TypeConversion.int_to_bool_to_int(-1) == 1
    assert TypeConversion.int_to_bool_to_int(-10) == 1
  end
end