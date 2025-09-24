defmodule MajorityElementFinder do
def find_majority_element(nums) do
    # Initialize candidate and count
    {candidate, count} = Enum.reduce(nums, {nil, 0}, fn num, {current_candidate, current_count} ->
      if current_count == 0 do
        {num, 1}
      else
        if num == current_candidate do
          {current_candidate, current_count + 1}
        else
          {current_candidate, current_count - 1}
        end
      end
    end)

    # Verify if the candidate is indeed the majority element
    if Enum.count(nums, &(&1 == candidate)) > div(length(nums), 2) do
      candidate
    else
      nil
    end
  end
end

# Test cases


ExUnit.start()
defmodule MajorityElementFinderTest do
use ExUnit.Case

test "finds the majority element" do
assert MajorityElementFinder.find_majority_element([3, 2, 3]) == 3
assert MajorityElementFinder.find_majority_element([2, 2, 1, 1, 1, 2, 2]) == 2
end

test "handles single element array" do
assert MajorityElementFinder.find_majority_element([1]) == 1
end

test "handles array where all elements are the same" do
assert MajorityElementFinder.find_majority_element([2, 2, 2]) == 2
end
end