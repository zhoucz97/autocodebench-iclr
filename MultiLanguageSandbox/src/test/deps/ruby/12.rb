

# Determines the maximum length of a string that can be accepted by a simplified regular expression.
# The function takes a single argument, regex, which is a string representing a regular expression 
# composed only of 'x', '(', ')', and '|'. The function returns the length of the longest string 
# that can be accepted by this regular expression.
#
# Example:
# >>> max_regex_string_length("((xx|xxx)x|(x|xx))xx")
# 6
# >>> max_regex_string_length("x(xx|x)x")
# 4

def max_regex_string_length(regex)
  # This function will parse the regex string and compute the maximum possible length
  # The approach involves parsing the string into components and evaluating them recursively
  
  # We'll use a helper function to parse the string and return the maximum length along with the consumed characters
  def parse(s, index = 0)
    max_len = 0
    current_len = 0
    
    while index < s.length
      char = s[index]
      if char == 'x'
        current_len += 1
        index += 1
      elsif char == '('
        # Start of a group, parse until the corresponding ')'
        # Need to find the matching ')'
        balance = 1
        start = index + 1
        index += 1
        while index < s.length && balance > 0
          if s[index] == '('
            balance += 1
          elsif s[index] == ')'
            balance -= 1
          end
          index += 1 unless balance == 0
        end
        # Now, parse the content between '(' and ')'
        group_max, _ = parse(s, start)
        current_len += group_max
      elsif char == '|'
        # Alternative found, compare current_len with max_len and reset current_len for the next alternative
        max_len = [max_len, current_len].max
        current_len = 0
        index += 1
      else
        # Unexpected character, though per problem statement, only x, (, ), | are present
        break
      end
    end
    # After loop, add the last current_len to max_len
    max_len = [max_len, current_len].max
    [max_len, index]
  end
  
  max_len, _ = parse(regex)
  max_len
end

raise 'Test failed' unless max_regex_string_length("((xx|xxx)x|(x|xx))xx") == 6
raise 'Test failed' unless max_regex_string_length("x(xx|x)x") == 4
raise 'Test failed' unless max_regex_string_length("xx|x") == 2
raise 'Test failed' unless max_regex_string_length("(xx|x)(x|xx)") == 4
raise 'Test failed' unless max_regex_string_length("x") == 1
  

puts 'All tests passed!'