function length_of_last_word(s::String) :: Int

    """
    Calculate the length of the last word in a given string. 
    A word is defined as a maximal substring consisting exclusively of letters, 
    separated by one or more spaces.
    
    # Examples
    - `length_of_last_word("Good morning everyone")` should return `8` (length of "everyone").
    - `length_of_last_word("JuliaProgramming")` should return `16` (length of the entire string).
    - `length_of_last_word(" ")` should return `0` (no words in the string).
    """
    # Split the string into words using whitespace as the delimiter
    words = split(s)
    
    # If there are no words, return 0
    if isempty(words)
        return 0
    end
    
    # Return the length of the last word
    return length(last(words))
end
@assert length_of_last_word("Good morning everyone") == 8
@assert length_of_last_word("JuliaProgramming") == 16
@assert length_of_last_word(" ") == 0
@assert length_of_last_word("Hello Julia World  ") == 5
@assert length_of_last_word("a") == 1