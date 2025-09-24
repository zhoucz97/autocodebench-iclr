

# Finds M positive integers whose sum is N, such that their product is maximized.
# The function returns an array of these integers in lexicographically smallest order.
#
# Args:
#   n: A positive integer representing the sum of the integers to find.
#   m: A positive integer representing the number of integers to find.
#
# Returns:
#   An array of integers that are the lexicographically smallest set of integers
#   whose sum is N and whose product is maximized.
#
# Examples:
#   >>> max_product_partition(6, 3)
#   [2, 2, 2]
#   >>> max_product_partition(8, 3)
#   [2, 3, 3]


def max_product_partition(n, m)
  return [] if n < m || m <= 0

  quotient = n / m
  remainder = n % m

  # The optimal partition is (quotient + 1) repeated 'remainder' times,
  # followed by 'quotient' repeated (m - remainder) times.
  # But to get lex smallest, we need to have the smaller numbers first.
  # So the array should be [quotient] * (m - remainder) + [quotient + 1] * remainder
  # But since quotient + 1 > quotient, the lex smallest is when the smaller numbers come first.
  # So the correct order is [quotient] (m - remainder times), then [quotient + 1] (remainder times).

  result = []
  (m - remainder).times { result << quotient }
  remainder.times { result << quotient + 1 }

  result
end

raise 'Test failed' unless max_product_partition(6, 3) == [2, 2, 2]
raise 'Test failed' unless max_product_partition(8, 3) == [2, 3, 3]
raise 'Test failed' unless max_product_partition(10, 2) == [5, 5]
raise 'Test failed' unless max_product_partition(7, 3) == [2, 2, 3]
raise 'Test failed' unless max_product_partition(20, 5) == [4, 4, 4, 4, 4]