

# Finds the longest palindrome within a given string.
# The function takes a single argument, str, which is a string that may include letters, numbers, and symbols.
# It returns the length of the longest palindrome that can be found within the string.
# Palindromes are sequences that read the same backward as forward.
# Note: The function is case-sensitive and considers each character.
#
# Example:
# >>> find_longest_palindrome("ABBA")
# 4
# >>> find_longest_palindrome("12ABBA")
# 4
# >>> find_longest_palindrome("A")
# 1
# >>> find_longest_palindrome("ABAKK")
# 3
# >>> find_longest_palindrome("51233214")
# 6
# >>> find_longest_palindrome("abaaab")
# 5


def find_longest_palindrome(str)
  return 0 if str.empty?
  
  max_length = 1
  
  (0...str.length).each do |i|
    # Odd length palindromes
    left, right = i, i
    while left >= 0 && right < str.length && str[left] == str[right]
      current_length = right - left + 1
      max_length = [max_length, current_length].max
      left -= 1
      right += 1
    end
    
    # Even length palindromes
    left, right = i, i + 1
    while left >= 0 && right < str.length && str[left] == str[right]
      current_length = right - left + 1
      max_length = [max_length, current_length].max
      left -= 1
      right += 1
    end
  end
  
  max_length
end

raise 'Test failed' unless find_longest_palindrome("ABBA") == 4
raise 'Test failed' unless find_longest_palindrome("12ABBA") == 4
raise 'Test failed' unless find_longest_palindrome("A") == 1
raise 'Test failed' unless find_longest_palindrome("ABAKK") == 3
raise 'Test failed' unless find_longest_palindrome("51233214") == 6
raise 'Test failed' unless find_longest_palindrome("abaaab") == 5


puts 'All tests passed!'