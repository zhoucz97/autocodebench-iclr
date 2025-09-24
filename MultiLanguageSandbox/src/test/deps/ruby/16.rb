

# Computes the repeated sum of digits of a given number string until it reduces to a single digit.
# The function takes a single argument, `input_string`, which is a string representing a non-negative number.
# It repeatedly sums the digits of the number until the sum is a single digit, then returns this single digit.
#
# Example:
# >>> digit_sum_to_single("35379")
# 9
# >>> digit_sum_to_single("24815")
# 3


def digit_sum_to_single(input_string)
  num = input_string.to_i
  while num >= 10
    sum = 0
    num.to_s.each_char do |c|
      sum += c.to_i
    end
    num = sum
  end
  num
end

raise 'Test failed' unless digit_sum_to_single("35379") == 9
raise 'Test failed' unless digit_sum_to_single("24815") == 2
raise 'Test failed' unless digit_sum_to_single("9999") == 9
raise 'Test failed' unless digit_sum_to_single("12345") == 6
raise 'Test failed' unless digit_sum_to_single("0") == 0
  

puts 'All tests passed!'