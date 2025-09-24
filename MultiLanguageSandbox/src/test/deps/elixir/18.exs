defmodule FishSwimmingUtils do
def will_fish_be_in_danger?(s, x) do
    initial_speed = 7.0
    threshold = 0.0001
    max_iterations = 1000  # To prevent infinite loops in case of unexpected behavior
    
    simulate(initial_speed, 0.0, s, x, threshold, 0, max_iterations)
  end
  
  defp simulate(_current_speed, total_distance, s, x, _threshold, _iterations, _max_iterations) 
       when total_distance >= s - x and total_distance <= s + x do
    'y'
  end
  
  defp simulate(current_speed, total_distance, s, x, threshold, iterations, max_iterations) 
       when iterations >= max_iterations do
    'n'
  end
  
  defp simulate(current_speed, total_distance, s, x, threshold, iterations, max_iterations) 
       when current_speed < threshold do
    'n'
  end
  
  defp simulate(current_speed, total_distance, s, x, threshold, iterations, max_iterations) do
    new_total = total_distance + current_speed
    new_speed = current_speed * 0.98
    simulate(new_speed, new_total, s, x, threshold, iterations + 1, max_iterations)
  end
end



ExUnit.start()

defmodule FishSwimmingUtilsTest do
use ExUnit.Case

test "fish escapes the danger zone" do
assert FishSwimmingUtils.will_fish_be_in_danger?(14, 1) == 'n'
assert FishSwimmingUtils.will_fish_be_in_danger?(20, 1.5) == 'n'
end

test "fish does not escape the danger zone" do
assert FishSwimmingUtils.will_fish_be_in_danger?(13, 2) == 'n'
end

test "edge cases" do
assert FishSwimmingUtils.will_fish_be_in_danger?(15, 0.5) == 'n'
assert FishSwimmingUtils.will_fish_be_in_danger?(50, 5) == 'y'
end
end