function p_type_encoding(str::String) :: String
"""
Encode a string of digits into its p-type encoding. The p-type encoding of a string describes
each sequence of consecutive identical digits by stating the count followed by the digit. 
For example, "122344111" becomes "1122132431", which means "1 one, 2 twos, 1 three, 2 fours, 3 ones". 
Similarly, "00000" becomes "50", representing "5 zeros".

# Examples
```julia
julia> p_type_encoding("122344111")
"1122132431"

julia> p_type_encoding("11111")
"51"

julia> p_type_encoding("121212")
"11121212"
"""
    if isempty(str)
        return ""
    end
    
    result = []
    current_char = str[1]
    count = 1
    
    for char in str[2:end]
        if char == current_char
            count += 1
        else
            push!(result, string(count))
            push!(result, current_char)
            current_char = char
            count = 1
        end
    end
    
    # Add the last sequence
    push!(result, string(count))
    push!(result, current_char)
    
    return join(result)
end
@assert p_type_encoding("122344111") == "1122132431"
@assert p_type_encoding("11111") == "51"
@assert p_type_encoding("121212") == "111211121112"
@assert p_type_encoding("00000") == "50"
@assert p_type_encoding("100200300") == "112012201320"