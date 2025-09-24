function find_substring_index(haystack::String, needle::String) :: Int
"""
Finds the first occurrence of the substring 'needle' in the string 'haystack'.
Returns the index of the first character of the first occurrence of 'needle'.
If 'needle' is not a part of 'haystack', returns -1.

# Examples
```julia
find_substring_index("hello world", "world")  # returns 7
find_substring_index("hello world", "bye")    # returns -1
"""
    len_haystack = length(haystack)
    len_needle = length(needle)
    
    if len_needle == 0
        return 1  # or handle empty needle case as per requirements
    end
    
    for i in 1:(len_haystack - len_needle + 1)
        if haystack[i:i+len_needle-1] == needle
            return i
        end
    end
    
    return -1
end
# Test cases
@assert find_substring_index("hello world", "world") == 7
@assert find_substring_index("hello world", "bye") == -1
@assert find_substring_index("abcdabcd", "bc") == 2
@assert find_substring_index("abcd", "") == 1