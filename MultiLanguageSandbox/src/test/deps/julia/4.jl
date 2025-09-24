function is_valid_brackets(s::String) :: Bool

"""
    Check if a given string of brackets is valid. A string is considered valid if 
    all types of brackets are correctly closed and in the correct order.
    
    # Examples
    >>> is_valid_brackets("[]{}()")
    True
    >>> is_valid_brackets("([)]")
    False
    >>> is_valid_brackets("{[()]}")
    True
    >>> is_valid_brackets("{{[[(())]]}}")
    True
"""
    stack = Char[]
    bracket_pairs = Dict(')' => '(', '}' => '{', ']' => '[')
    
    for c in s
        if c in values(bracket_pairs)
            push!(stack, c)
        elseif c in keys(bracket_pairs)
            if isempty(stack) || pop!(stack) != bracket_pairs[c]
                return false
            end
        else
            # Ignore non-bracket characters (though the problem says it's a bracket string)
            continue
        end
    end
    
    return isempty(stack)
end
# Test cases
@assert is_valid_brackets("()") == true
@assert is_valid_brackets("()[]{}") == true
@assert is_valid_brackets("(]") == false
@assert is_valid_brackets("([)]") == false
@assert is_valid_brackets("{[]}") == true
@assert is_valid_brackets("") == true
@assert is_valid_brackets("{[()]}") == true
@assert is_valid_brackets("{{[[(())]]}}") == true