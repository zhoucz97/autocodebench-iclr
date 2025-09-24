

# Finds the largest letter(s) in the input string (considering 'A' and 'a' as the same letter) and inserts "(max)" after each of its occurrences.
# The function takes one argument, input_string, which is a string of letters and possibly digits.
# Returns a new string with "(max)" inserted after each occurrence of the largest letter(s).
#
# Examples:
# >>> insert_max_after_largest_letters("abcdefgfedcba")
# "abcdefg(max)fedcba"
# >>> insert_max_after_largest_letters("aAbBcC")
# "aAbBc(max)C(max)"

def insert_max_after_largest_letters(input_string)
  # Convert all letters to lowercase to compare them case-insensitively
  lower_case_string = input_string.downcase
  
  # Find the largest letter (case-insensitive)
  largest_letter = lower_case_string.chars.max
  
  # Insert "(max)" after each occurrence of the largest letter (case-sensitive)
  result = input_string.gsub(largest_letter) { |match| "#{match}(max)" }
  
  result
end

raise 'Test failed' unless insert_max_after_largest_letters("abcdefgfedcba") == "abcdefg(max)fedcba"
raise 'Test failed' unless insert_max_after_largest_letters("xxxxx") == "x(max)x(max)x(max)x(max)x(max)"
raise 'Test failed' unless insert_max_after_largest_letters("Hello World!") == "Hello W(max)orld!"
raise 'Test failed' unless insert_max_after_largest_letters("Ruby123") == "Ruby(max)123"
raise 'Test failed' unless insert_max_after_largest_letters("") == ""
raise 'Test failed' unless insert_max_after_largest_letters("aAbBcC") == "aAbBc(max)C(max)"
  

puts 'All tests passed!'