function remove_specific_elements!(nums::Vector{Int}, val::Int) :: Int
 """
Remove all instances of a specified value from an array and return the new length of the array.
The array is modified in place to save space. The order of the elements can be changed.

# Examples
>>> remove_specific_elements!([3, 1, 2, 3, 4], 3)
4  # [1, 2, 4, 3] or any other order of [1, 2, 4]

>>> remove_specific_elements!([2, 2, 3, 2, 2, 4, 2, 5], 2)
3  # [3, 4, 5] or any other order of [3, 4, 5]
"""
    write_index = 1
    for read_index in 1:length(nums)
        if nums[read_index] != val
            nums[write_index] = nums[read_index]
            write_index += 1
        end
    end
    return write_index - 1
end
# Test cases
@assert remove_specific_elements!([3, 1, 2, 3, 4], 3) == 3
@assert remove_specific_elements!([2, 2, 3, 2, 2, 4, 2, 5], 2) == 3
@assert remove_specific_elements!([1, 1, 1, 1, 1], 1) == 0
@assert remove_specific_elements!([5, 6, 7, 8, 9], 10) == 5