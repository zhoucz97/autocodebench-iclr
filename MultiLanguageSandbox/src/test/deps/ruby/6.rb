

# Swaps the case of each letter in a given string.
# The function takes a single argument, input_string, which is a string containing only English alphabets without any spaces or other types of characters. 
# It returns a new string where every lowercase letter is converted to uppercase and every uppercase letter is converted to lowercase.
#
# Example:
# >>> swap_case("HelloWorld")
# "hELLOwORLD"
# >>> swap_case("Ruby")
# "rUBY"


def swap_case(input_string)
  input_string.chars.map { |c| c == c.upcase ? c.downcase : c.upcase }.join
end

raise 'Test failed' unless swap_case("HelloWorld") == "hELLOwORLD"
raise 'Test failed' unless swap_case("Ruby") == "rUBY"
raise 'Test failed' unless swap_case("AeDb") == "aEdB"
raise 'Test failed' unless swap_case("JAVA") == "java"
raise 'Test failed' unless swap_case("python") == "PYTHON"
  

puts 'All tests passed!'