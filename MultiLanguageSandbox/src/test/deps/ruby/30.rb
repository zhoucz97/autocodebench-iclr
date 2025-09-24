

# Calculates the number of possible photo arrangements for a group of people.
# In this arrangement, people are lined up in such a way that the age difference
# between any two adjacent persons is no more than two years.
# The function takes a single integer argument, n, representing the number of people
# in the group, where each person has a distinct age from 1 to n years.
#
# Example:
# >>> photo_arrangements_count(4)
# 4
# >>> photo_arrangements_count(5)
# 6

def photo_arrangements_count(n)
  return 0 if n == 0
  return 1 if n == 1

  # We'll use memoization to store the number of ways to arrange the remaining numbers
  # with the last number being 'last' and the set of used numbers.
  # However, for n up to 10 or so, this might be feasible with memoization.
  # For larger n, a more optimized approach is needed, but given the examples,
  # perhaps n is small.

  # Initialize a memo hash to store intermediate results
  memo = {}

  def dfs(used, last, n, memo)
    key = [used.sort, last]
    return memo[key] if memo.key?(key)

    if used.size == n
      return 1
    end

    count = 0
    (1..n).each do |num|
      next if used.include?(num)
      if used.empty? || (num - last).abs <= 2
        new_used = used.dup
        new_used << num
        count += dfs(new_used, num, n, memo)
      end
    end

    memo[key] = count
    count
  end

  total = 0
  (1..n).each do |start|
    used = [start]
    total += dfs(used, start, n, memo)
  end

  total
end

  
raise 'Test failed' unless photo_arrangements_count(4) == 4
raise 'Test failed' unless photo_arrangements_count(5) == 6
raise 'Test failed' unless photo_arrangements_count(6) == 9
raise 'Test failed' unless photo_arrangements_count(7) == 14
raise 'Test failed' unless photo_arrangements_count(8) == 21
  

puts 'All tests passed!'