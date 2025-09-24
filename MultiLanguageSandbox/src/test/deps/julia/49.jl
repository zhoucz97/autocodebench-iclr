function find_error_ids(ids::Vector{Int}) :: Tuple{Int, Int}
    """
    Given a vector of ID numbers where ID numbers are supposed to be consecutive but due to an error, one ID is missing (creating a gap) and one ID is duplicated (creating a repeat). This function finds the missing (gap) ID and the duplicated (repeat) ID.

    # Examples
    - `find_error_ids([5, 6, 8, 11, 9, 10, 12, 9])` should return `(7, 9)`, indicating that 7 is missing and 9 is repeated.
    - `find_error_ids([1, 2, 4, 4, 5])` should return `(3, 4)`, indicating that 3 is missing and 4 is repeated.
    """
    sorted_ids = sort(ids)
    n = length(sorted_ids)
    missing_id = -1
    duplicate_id = -1
    
    for i in 2:n
        if sorted_ids[i] == sorted_ids[i-1]
            duplicate_id = sorted_ids[i]
        elseif sorted_ids[i] > sorted_ids[i-1] + 1
            missing_id = sorted_ids[i-1] + 1
        end
    end
    
    # Check if the first element is not 1 (assuming the sequence starts at 1)
    if sorted_ids[1] != 1
        missing_id = 1
    end
    
    # Check if the last element is not the expected last element (n + 1 if no missing)
    # But since there's one missing and one duplicate, the total elements should be n, which is (max - min + 1) - 1 + 1 = max - min + 1
    # So the expected max is min + n - 1 + 1 (since one is missing)
    # Wait, perhaps better to compute the expected max as min + n (since one is missing and one is duplicate, so the sequence should have n+1 elements without errors)
    # But the actual max is sorted_ids[end]
    # The expected max is sorted_ids[1] + n (because the correct sequence would have n+1 elements, but one is missing)
    # So if sorted_ids[end] != sorted_ids[1] + n, then the missing could be at the end.
    # But in the examples, the sequences are like 5..12 (8 elements), but one missing and one duplicate. So the correct sequence would be 5..12 (8 elements), but one missing means the correct should be 5..12 +1 (9 elements), but one is missing, so 8 elements. So the max is 12, which is correct.
    # So perhaps no need for this check.
    
    # Alternatively, another approach: the sum of the correct sequence minus the sum of the given sequence plus the duplicate should give the missing.
    # Let's try that.
    
    # Compute the expected sum if no errors: the sequence is from min to max, inclusive, with n+1 elements (since one is missing and one is duplicate, total elements is n, which is (max - min + 1) -1 +1 = max - min +1. So the correct sequence has max - min +1 elements, but one is missing, so the given has (max - min +1 -1 +1) = max - min +1 elements? No, wait.
    # The correct sequence without errors would have (max - min +1) elements. But with one missing and one duplicate, the given sequence has (max - min +1 -1 +1) = max - min +1 elements. So the sum of the correct sequence is sum_{i=min}^{max} i = (max + min)*(max - min +1)/2.
    # The sum of the given sequence is sum(ids) = correct_sum - missing + duplicate.
    # So missing = correct_sum - sum(ids) + duplicate.
    # But we don't know duplicate yet. So this approach may not help unless we find duplicate first.
    
    # Given the initial approach with sorting seems to work for the examples, let's proceed.
    
    # However, the initial approach may fail if the missing number is at the beginning or end. For example, [2, 2, 3] should return (1, 2). The initial loop won't catch the missing 1.
    # So need to handle the start and end cases.
    
    # Revising the approach:
    # The correct sequence should be from min to max, with (max - min +1) elements. But the given has (max - min +1) elements (since one is missing and one is duplicate).
    # So the sum of the correct sequence is S = (min + max)*(max - min +1)/2.
    # The sum of the given is sum(ids) = S - missing + duplicate.
    # So missing = S - sum(ids) + duplicate.
    # But we don't know duplicate yet.
    
    # Alternative: the duplicate is the number that appears twice in the sorted array.
    # The missing is the number that is not present but should be in the sequence from min to max.
    
    # So steps:
    # 1. Find the duplicate by checking adjacent elements in the sorted array.
    # 2. The missing can be found by the expected sum method or by checking the sequence.
    
    # Implementing the initial approach with checks for start and end:
    
    # Find duplicate
    duplicate_id = -1
    for i in 2:n
        if sorted_ids[i] == sorted_ids[i-1]
            duplicate_id = sorted_ids[i]
            break
        end
    end
    
    # Find missing
    missing_id = -1
    if sorted_ids[1] != 1  # assuming the sequence starts at 1? No, the sequence could start at any min.
    # Better to find the min and max in the array.
    min_id = minimum(sorted_ids)
    max_id = maximum(sorted_ids)
    # The correct sequence should have elements from min_id to max_id, inclusive, which is (max_id - min_id +1) elements.
    # But the given has n elements, which is (max_id - min_id +1) because one is missing and one is duplicate.
    # So the sum of correct sequence is S = (min_id + max_id)*(max_id - min_id +1)/2.
    # Sum of given is sum(ids) = S - missing + duplicate.
    # So missing = S - sum(ids) + duplicate.
    S = (min_id + max_id) * (max_id - min_id + 1) ÷ 2
    sum_ids = sum(sorted_ids)
    missing_id = S - sum_ids + duplicate_id
    end
    
    return (missing_id, duplicate_id)
end
@assert find_error_ids([5, 6, 8, 11, 9, 10, 12, 9]) == (7, 9)
@assert find_error_ids([1, 2, 4, 4, 5]) == (3, 4)
@assert find_error_ids([10, 11, 12, 13, 14, 15, 16, 18, 17, 17]) == (19, 17)
@assert find_error_ids([22, 23, 24, 25, 27, 27]) == (26, 27)