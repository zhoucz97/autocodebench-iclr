

# Calculates the minimum number of character operations needed to transform one string into another.
# The function takes two string arguments, str_a and str_b. The operations allowed are:
# 1. Delete a character;
# 2. Insert a character;
# 3. Replace a character with another.
# It returns an integer representing the minimum number of operations required to transform str_a into str_b.
# Both strings should have a length less than 200.
#
# Example:
# >>> min_operations_to_transform("apple", "aple")
# 1
# >>> min_operations_to_transform("kitten", "sitting")
# 3

def min_operations_to_transform(str_a, str_b)
  m = str_a.length
  n = str_b.length

  # Create a DP table to store results of subproblems
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }

  # Fill dp[][] in bottom up manner
  (0..m).each do |i|
    (0..n).each do |j|
      if i == 0
        dp[i][j] = j  # Only option is to insert all characters of str_b
      elsif j == 0
        dp[i][j] = i  # Only option is to remove all characters of str_a
      elsif str_a[i-1] == str_b[j-1]
        dp[i][j] = dp[i-1][j-1]  # Characters match, no operation needed
      else
        dp[i][j] = 1 + [dp[i][j-1],    # Insert
                        dp[i-1][j],    # Remove
                        dp[i-1][j-1]].min  # Replace
      end
    end
  end

  dp[m][n]
end

raise 'Test failed' unless min_operations_to_transform("apple", "aple") == 1
raise 'Test failed' unless min_operations_to_transform("kitten", "sitting") == 3
raise 'Test failed' unless min_operations_to_transform("sfdxbqw", "gfdgw") == 4
raise 'Test failed' unless min_operations_to_transform("horse", "ros") == 3
raise 'Test failed' unless min_operations_to_transform("", "abc") == 3



puts 'All tests passed!'