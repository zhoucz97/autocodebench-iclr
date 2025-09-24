defmodule MaxProductFinder do
def max_product_partition(n, m) do
    if n < m do
      []
    else
      base = div(n, m)
      remainder = rem(n, m)
      
      # Create a list with 'base' repeated 'm' times, then add 1 to the first 'remainder' elements
      partition = 
        Enum.map(0..(m-1), fn i ->
          if i < remainder do
            base + 1
          else
            base
          end
        end)
      
      partition
    end
  end
end



ExUnit.start()
defmodule MaxProductFinderTest do
use ExUnit.Case

test "finds numbers with max product" do
assert MaxProductFinder.max_product_partition(8, 3) == [2, 3, 3]
assert MaxProductFinder.max_product_partition(10, 2) == [5, 5]
end

test "handles impossible partitions" do
assert MaxProductFinder.max_product_partition(3, 5) == []
end

test "handles single number partition" do
assert MaxProductFinder.max_product_partition(5, 1) == [5]
end
end