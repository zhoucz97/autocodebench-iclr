defmodule ManureTransportUtils do
def min_distance(a, b, x, y) do
    # Option 1: Direct path from a to b
    direct_distance = abs(b - a)
    
    # Option 2: Path from a to x, teleport to y, then to b
    path_via_x = abs(x - a) + abs(y - b)
    
    # Option 3: Path from a to y, teleport to x, then to b
    path_via_y = abs(y - a) + abs(x - b)
    
    # Return the minimum of the three options
    min(direct_distance, path_via_x, path_via_y)
  end

  defp min(a, b, c) do
    min(min(a, b), c)
  end
end

# Example usage:


ExUnit.start()

defmodule ManureTransportUtilsTest do
use ExUnit.Case

test "calculates minimum distance with portal being useful" do
assert ManureTransportUtils.min_distance(3, 10, 8, 2) == 3
end

test "calculates minimum distance with portal not being useful" do
assert ManureTransportUtils.min_distance(5, 15, 10, 20) == 10
end

test "calculates minimum distance with start and end points being the same" do
assert ManureTransportUtils.min_distance(5, 5, 10, 20) == 0
end

test "calculates minimum distance with portal being exactly on the route" do
assert ManureTransportUtils.min_distance(0, 20, 5, 10) == 15
end
end