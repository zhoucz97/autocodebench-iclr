function find_insert_position(nums::Vector{Int}, target::Int) :: Int

    """
    Find the index at which a target value should be inserted into a sorted array to maintain order, or the index of the target value if it is already in the array.
    
    This function uses a binary search algorithm for an O(log n) time complexity.
    
    # Examples
    - `find_insert_position([1, 3, 5, 6], 4)` should return `2`.
    - `find_insert_position([2, 4, 6, 8, 10], 7)` should return `3`.
    - `find_insert_position([10, 20, 30], 15)` should return `1`.
    """
    left, right = 1, length(nums)
    
    while left <= right
        mid = (left + right) ÷ 2
        if nums[mid] == target
            return mid
        elseif nums[mid] < target
            left = mid + 1
        else
            right = mid - 1
        end
    end
    
    return left
end
# Test cases
@assert find_insert_position([1, 3, 5, 6], 4) == 3
@assert find_insert_position([2, 4, 6, 8, 10], 7) == 4
@assert find_insert_position([10, 20, 30], 15) == 2
@assert find_insert_position([1, 3, 5, 6], 6) == 4
@assert find_insert_position([5, 10, 15], 1) == 1