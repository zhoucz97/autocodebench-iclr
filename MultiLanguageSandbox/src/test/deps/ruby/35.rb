

# Finds all possible last two digits of a number that, when added to the number 'a',
# makes it divisible by 'b'. The function takes two arguments, 'a' and 'b', where 'a' is 
# a positive integer less than 10000, and 'b' is a positive integer between 10 and 100.
# The function returns an array of strings representing the possible two-digit suffixes.
#
# Example:
# >>> find_possible_last_digits(200, 40)
# ["00", "40", "80"]
# >>> find_possible_last_digits(1992, 95)
# ["15"]

def find_possible_last_digits(a, b)
  possible_suffixes = []
  (0..99).each do |x|
    if (a + x) % b == 0
      # Format the number as a two-digit string with leading zero if necessary
      suffix = "%02d" % x
      possible_suffixes << suffix
    end
  end
  possible_suffixes
end

raise 'Test failed' unless find_possible_last_digits(200, 40) == ["00", "40", "80"]
raise 'Test failed' unless find_possible_last_digits(1992, 95) == ["15"]
raise 'Test failed' unless find_possible_last_digits(1500, 25) == ["00", "25", "50", "75"]
raise 'Test failed' unless find_possible_last_digits(300, 30) == ["00", "30", "60", "90"]
  
puts 'All tests passed!'