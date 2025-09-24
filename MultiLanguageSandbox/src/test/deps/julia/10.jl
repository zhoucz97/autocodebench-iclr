function add_binary_strings(a::String, b::String) :: String

"""
    Add two binary strings and return their sum as a binary string.

    # Examples
    >>> add_binary_strings("110", "1001")
    "1111"

    >>> add_binary_strings("0", "1")
    "1"

    >>> add_binary_strings("101", "110")
    "1011"
"""
    # Pad the shorter string with leading zeros to make them the same length
    max_len = max(length(a), length(b))
    a = lpad(a, max_len, '0')
    b = lpad(b, max_len, '0')
    
    carry = 0
    result = []
    
    # Iterate from the end of both strings to the beginning
    for i in max_len:-1:1
        digit_a = parse(Int, a[i])
        digit_b = parse(Int, b[i])
        
        total = digit_a + digit_b + carry
        push!(result, string(total % 2))
        carry = total ÷ 2
    end
    
    # If there's any carry left, add it to the result
    if carry > 0
        push!(result, string(carry))
    end
    
    # Reverse the result and join into a string
    return join(reverse(result))
end
@assert add_binary_strings("110", "1001") == "1111"
@assert add_binary_strings("0", "1") == "1"
@assert add_binary_strings("101", "110") == "1011"
@assert add_binary_strings("111", "111") == "1110"
@assert add_binary_strings("10101", "1100") == "100001"