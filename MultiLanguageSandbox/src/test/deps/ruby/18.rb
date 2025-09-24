

# Calculates the final minimum weight of fish in a simulated feeding game.
# In this game, there are 'n' fish, each with an initial weight given in the 'weights' array.
# The game proceeds for 'm' rounds. In each round, the lightest fish eats the second lightest,
# and its weight becomes the sum of both. The function returns the weight of the lightest fish
# after 'm' rounds.
#
# Arguments:
# n (Integer) -- The number of fish.
# m (Integer) -- The number of rounds the game will be played.
# weights (Array of Integers) -- An array representing the weights of each fish.
#
# Example:
# >>> final_min_weight_fish(1, [1, 2, 3])
# 3
# >>> final_min_weight_fish(2, [4, 3, 2, 1])
# 4

def final_min_weight_fish(m, weights)
  # Use a min-heap to efficiently get the two smallest elements
  heap = weights.sort
  m.times do
    if heap.size < 2
      break # not enough fish to perform another merge
    end
    # Extract the two smallest elements
    first = heap.shift
    second = heap.shift
    # Merge them and add back to the heap
    merged = first + second
    # Insert the merged weight in the correct position to maintain the sorted order
    # Since Ruby's Array#bsearch_index can help find the insertion point
    index = heap.bsearch_index { |x| x >= merged } || heap.size
    heap.insert(index, merged)
  end
  heap.min
end

raise 'Test failed' unless final_min_weight_fish(1, [1, 2, 3]) == 3
raise 'Test failed' unless final_min_weight_fish(2, [4, 3, 2, 1]) == 4
raise 'Test failed' unless final_min_weight_fish(2, [5, 4, 3, 2, 1]) == 4
raise 'Test failed' unless final_min_weight_fish(2, [3, 2, 1]) == 6
raise 'Test failed' unless final_min_weight_fish(3, [1, 2, 3, 4]) == 10
  
puts 'All tests passed!'