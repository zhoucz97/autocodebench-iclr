

# Calculates the minimum number of square tiles required to completely cover a square room of size n x n.
# The tiles used must be smaller than n x n and can be of varying sizes. 
# The function takes a single argument, n, which is a positive integer representing the size of the room, 
# and returns the minimum number of tiles required.
#
# Example:
# >>> min_tiles_required(4)
# 4
# >>> min_tiles_required(5)
# 8


def min_tiles_required(n)
  if n % 2 == 0
    (n / 2) ** 2
  else
    2 * n - 2
  end
end

raise 'Test failed' unless min_tiles_required(4) == 4
raise 'Test failed' unless min_tiles_required(5) == 8
raise 'Test failed' unless min_tiles_required(2) == 4
raise 'Test failed' unless min_tiles_required(10) == 4
raise 'Test failed' unless min_tiles_required(7) == 10
  
puts 'All tests passed!'   