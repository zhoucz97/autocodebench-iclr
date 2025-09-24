function longest_common_substring_in_rings(ring1::String, ring2::String) :: Int
"""
Determines the length of the longest continuous common substring in two character rings.
A character ring is a string where the end connects back to the beginning, forming a loop.
The function takes two strings representing the rings and returns the length of the longest common substring.

Examples:
>>> longest_common_substring_in_rings("ABCDEFABC", "FABCDE")
6
>>> longest_common_substring_in_rings("XYZABCDXYZ", "ABCD")
9
"""
    # Double the rings to handle circular nature
    s1 = ring1 * ring1
    s2 = ring2 * ring2
    
    m, n = length(s1), length(s2)
    max_len = 0
    # Create a DP table initialized to zero
    dp = zeros(Int, m + 1, n + 1)
    
    for i in 1:m
        for j in 1:n
            if s1[i] == s2[j]
                dp[i+1, j+1] = dp[i, j] + 1
                if dp[i+1, j+1] > max_len
                    max_len = dp[i+1, j+1]
                end
            else
                dp[i+1, j+1] = 0
            end
        end
    end
    
    return max_len
end
@assert longest_common_substring_in_rings("ABCEFAGADEGKABUVKLM", "MADJKLUVKL") == 6
@assert longest_common_substring_in_rings("ABCDEFABC", "FABCDE") == 9