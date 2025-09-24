function longest_common_prefix(strs::Vector{String}) :: String
"""
Find the longest common prefix among a vector of strings.

# Examples
- `longest_common_prefix(["flower", "flow", "flight"])` should return "fl".
- `longest_common_prefix(["dog", "racecar", "car"])` should return "" (no common prefix).
"""
    if isempty(strs)
        return ""
    end
    
    # The shortest string in the array will be the maximum possible prefix length
    min_len = minimum(length.(strs))
    
    prefix = ""
    for i in 1:min_len
        # Get the current character from the first string
        current_char = strs[1][i]
        
        # Check if all strings have the same character at this position
        for s in strs
            if s[i] != current_char
                return prefix
            end
        end
        
        # If all characters match, add to the prefix
        prefix *= current_char
    end
    
    return prefix
end
# Test cases
@assert longest_common_prefix(["flower", "flow", "flight"]) == "fl"
@assert longest_common_prefix(["dog", "racecar", "car"]) == ""
@assert longest_common_prefix(["interspace", "interstellar", "internet", "intermediate"]) == "inter"
@assert longest_common_prefix(["apple", "apricot", "apps"]) == "ap"
@assert longest_common_prefix(["", ""]) == ""
@assert longest_common_prefix(["single"]) == "single"