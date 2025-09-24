function count_chars_types(s::String):: Tuple{Int, Int, Int}
    """
    Count the number of digits, lowercase letters, and uppercase letters in a given string of length 8.

    # Arguments
    - `s`: A string of exactly 8 characters.

    # Returns
    - A tuple of three integers: the number of digits, the number of lowercase letters, and the number of uppercase letters in the string.

    # Examples
    count_chars_types("1n2s0e1s") == (4, 4, 0)
    """
    digits_count = 0
    lowercase_count = 0
    uppercase_count = 0

    for c in s
        if isdigit(c)
            digits_count += 1
        elseif islowercase(c)
            lowercase_count += 1
        elseif isuppercase(c)
            uppercase_count += 1
        end
    end

    return (digits_count, lowercase_count, uppercase_count)
end
@assert count_chars_types("yLOI2022") == (4, 1, 3)
@assert count_chars_types("IAKIOIOI") == (0, 0, 8)
@assert count_chars_types("1n2s0e1s") == (4, 4, 0)
@assert count_chars_types("1234AbCD") == (4, 1, 3)