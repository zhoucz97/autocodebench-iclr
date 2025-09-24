

# Determines the maximum number of drop tests needed to find the drop tolerance height of a device from a building with a given height.
# The function takes a single argument, n, which is the height of the building (number of floors).
# It returns the maximum number of tests needed to find the exact floor from which if the device is dropped, it will break.
#
# The problem assumes the worst-case scenario, where the outcome of each test is the least favorable, thus requiring the maximum number of tests.
#
# Example:
# >>> max_drop_test_height(3)
# 2
# >>> max_drop_test_height(10)
# 4


def max_drop_test_height(n)
  k = 0
  while (k * (k + 1)) / 2 < n
    k += 1
  end
  k
end

raise 'Test failed' unless max_drop_test_height(3) == 2
raise 'Test failed' unless max_drop_test_height(10) == 4
raise 'Test failed' unless max_drop_test_height(15) == 5
raise 'Test failed' unless max_drop_test_height(6) == 3
raise 'Test failed' unless max_drop_test_height(100) == 9


puts 'All tests passed!'