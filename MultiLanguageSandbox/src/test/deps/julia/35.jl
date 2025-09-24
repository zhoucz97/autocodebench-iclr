function is_lucky_word(word::String)::Tuple{String, Int64}
    """
    Determine if a word is a Lucky Word and calculate maxn - minn.
    
    A word is considered a Lucky Word if the difference between the maximum and minimum
    frequencies of letters in the word is a prime number.
    
    Args:
    - word (String): The input word containing only lowercase letters.
    
    Returns:
    - Tuple{String, Int64}: A tuple containing two elements:
      - If the word is a Lucky Word, the first element is "Lucky Word", otherwise "No Answer".
      - The second element is the value of maxn - minn, where maxn is the maximum frequency of
        a letter in the word, and minn is the minimum frequency of a letter in the word.
    
    Examples:
    >>> is_lucky_word("error")
    ("Lucky Word", 2)
    
    >>> is_lucky_word("olympic")
    ("Lucky Word", 1)
    
    >>> is_lucky_word("programming")
    ("No Answer", 0)
    """
    # Count the frequency of each letter in the word
    freq = Dict{Char, Int64}()
    for c in word
        freq[c] = get(freq, c, 0) + 1
    end
    
    # Get the frequencies as a list
    frequencies = collect(values(freq))
    
    # Calculate maxn and minn
    maxn = maximum(frequencies)
    minn = minimum(frequencies)
    diff = maxn - minn
    
    # Check if the difference is a prime number
    function is_prime(n::Int64)::Bool
        if n <= 1
            return false
        end
        if n == 2
            return true
        end
        if n % 2 == 0
            return false
        end
        for i in 3:2:isqrt(n)
            if n % i == 0
                return false
            end
        end
        return true
    end
    
    if is_prime(diff)
        return ("Lucky Word", diff)
    else
        return ("No Answer", diff)
    end
end
# Test cases for is_lucky_word function
@assert is_lucky_word("error") == ("Lucky Word", 2)
@assert is_lucky_word("olympic") == ("No Answer", 0)
@assert is_lucky_word("programming") == ("No Answer", 0)
@assert is_lucky_word("banana") == ("Lucky Word", 2)