function merge_sorted_arrays(nums1::Vector{Int}, m::Int, nums2::Vector{Int}, n::Int) :: Vector{Int}
    """
    Merge two sorted arrays, `nums1` and `nums2`, where `nums1` has a size of `m + n`, with the first `m` elements containing values and the last `n` elements set to zero, representing space allocated for merging. `nums2` has `n` elements. The merged array should maintain non-decreasing order.
    
    Examples:
    >>> merge_sorted_arrays([1, 3, 5, 0, 0, 0], 3, [2, 4, 6], 3)
    [1, 2, 3, 4, 5, 6]
    >>> merge_sorted_arrays([4, 0], 1, [3], 1)
    [3, 4]
    >>> merge_sorted_arrays([0], 0, [1], 1)
    [1]
    """
    # Initialize pointers for nums1, nums2, and the merged array
    i = m - 1  # Last element of nums1's valid part
    j = n - 1  # Last element of nums2
    k = m + n - 1  # Last position of the merged array

    # Merge from the end to avoid overwriting nums1's elements
    while i >= 0 && j >= 0
        if nums1[i] > nums2[j]
            nums1[k] = nums1[i]
            i -= 1
        else
            nums1[k] = nums2[j]
            j -= 1
        end
        k -= 1
    end

    # If there are remaining elements in nums2, copy them
    while j >= 0
        nums1[k] = nums2[j]
        j -= 1
        k -= 1
    end

    return nums1
end
@assert merge_sorted_arrays([1, 3, 5, 0, 0, 0], 3, [2, 4, 6], 3) == [1, 2, 3, 4, 5, 6]
@assert merge_sorted_arrays([4, 0], 1, [3], 1) == [3, 4]
@assert merge_sorted_arrays([0], 0, [1], 1) == [1]
@assert merge_sorted_arrays([2, 3, 0, 0, 0], 2, [1, 4, 5], 3) == [1, 2, 3, 4, 5]