defmodule NumberUtils do
def has_close_elements(numbers, threshold) do
    # Sort the numbers first to easily find adjacent elements
    sorted = Enum.sort(numbers)
    
    # Check all adjacent pairs in the sorted list
    Enum.chunk_every(sorted, 2, 1, :discard)
    |> Enum.any?(fn [a, b] -> abs(a - b) < threshold end)
  end
end



ExUnit.start()
defmodule NumberUtilsTest do
use ExUnit.Case

test "has close elements with threshold 0.3" do
assert NumberUtils.has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3)
refute NumberUtils.has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05)
end

test "has close elements with threshold 0.95" do
assert NumberUtils.has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95)
refute NumberUtils.has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8)
end

test "has close elements with various thresholds" do
assert NumberUtils.has_close_elements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1)
assert NumberUtils.has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0)
refute NumberUtils.has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5)
end
end