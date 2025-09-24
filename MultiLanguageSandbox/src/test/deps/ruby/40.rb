

# Calculates the time needed to hang a saline solution.
#
# The function takes two arguments: vul, which is the total volume of the saline solution in milliliters,
# and d, which is the volume of each drop in milliliters. The function assumes that each drop takes 
# one second to fall and there is a one-second pause after a certain number of drops, which increases 
# sequentially (1 drop, then a pause, 2 drops, then a pause, and so on).
# The function returns the total time in seconds required to hang the entire volume of saline solution.
#
# Examples:
# >>> calculate_hanging_time(10, 1)
# 13
# >>> calculate_hanging_time(20, 2)
# 13


def calculate_hanging_time(vul, d)
  total_drops = vul / d
  time = 0
  sequence = 1

  while total_drops > 0
    drops_in_sequence = [sequence, total_drops].min
    time += drops_in_sequence  # time for the drops
    total_drops -= drops_in_sequence

    if total_drops > 0
      time += 1  # pause after the sequence
    end

    sequence += 1
  end

  time
end

raise 'Test failed' unless calculate_hanging_time(10, 1) == 13
raise 'Test failed' unless calculate_hanging_time(20, 2) == 13
raise 'Test failed' unless calculate_hanging_time(15, 3) == 7
raise 'Test failed' unless calculate_hanging_time(50, 5) == 13
raise 'Test failed' unless calculate_hanging_time(100, 10) == 13

  
puts 'All tests passed!'