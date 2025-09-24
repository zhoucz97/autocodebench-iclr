

# Counts the number of digits, lowercase letters, and uppercase letters in a given string.
# The function takes a single argument, s, which is a string of length 8,
# and returns an array with three integers representing the count of digits,
# lowercase letters, and uppercase letters in the string, respectively.
#
# Examples:
# >>> count_chars_types("yLOI2022")
# [4, 1, 3]
# >>> count_chars_types("IAKIOIOI")
# [0, 0, 8]
# >>> count_chars_types("1n2s0e1s")
# [4, 4, 0]
def count_chars_types(s)
  digit_count = 0
  lower_count = 0
  upper_count = 0

  s.each_char do |char|
    if char.match?(/\d/)
      digit_count += 1
    elsif char.match?(/[a-z]/)
      lower_count += 1
    elsif char.match?(/[A-Z]/)
      upper_count += 1
    end
  end

  [digit_count, lower_count, upper_count]
end

raise 'Test failed' unless count_chars_types("yLOI2022") == [4, 1, 3]
raise 'Test failed' unless count_chars_types("IAKIOIOI") == [0, 0, 8]
raise 'Test failed' unless count_chars_types("1n2s0e1s") == [4, 4, 0]
raise 'Test failed' unless count_chars_types("12345678") == [8, 0, 0]
raise 'Test failed' unless count_chars_types("abcdefgh") == [0, 8, 0]