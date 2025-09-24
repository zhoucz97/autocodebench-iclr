

# Calculates the number of unique permutations of a given string.
# The function takes a single argument, str, which is a string consisting of
# lowercase letters. It returns the total number of unique permutations that
# can be formed with the characters in str.
#
# Example:
# >>> unique_permutations_count("aacc")
# 6
# >>> unique_permutations_count("abc")
# 6

def unique_permutations_count(str)
  char_counts = Hash.new(0)
  str.each_char { |c| char_counts[c] += 1 }
  
  total = factorial(str.length)
  denominator = 1
  char_counts.each_value do |count|
    denominator *= factorial(count)
  end
  
  total / denominator
end

def factorial(n)
  return 1 if n <= 1
  (1..n).inject(:*)
end

raise 'Test failed' unless unique_permutations_count("aacc") == 6
raise 'Test failed' unless unique_permutations_count("abc") == 6
raise 'Test failed' unless unique_permutations_count("abcd") == 24
raise 'Test failed' unless unique_permutations_count("aaa") == 1
raise 'Test failed' unless unique_permutations_count("ab") == 2
raise 'Test failed' unless unique_permutations_count("aacdarwqea") == 151200

  

puts 'All tests passed!'