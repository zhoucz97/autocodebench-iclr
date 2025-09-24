function count_composition_title_chars(title::String) :: Int

    """
    Count the number of characters in a composition title, excluding spaces and newline characters.
    
    # Examples
    - `count_composition_title_chars("234")` returns `3`.
    - `count_composition_title_chars("Ca 45")` returns `4`.
    - `count_composition_title_chars("Hello\nWorld")` returns `10`.
    - `count_composition_title_chars(" A B C ")` returns `3`.
    """
    # Filter out spaces and newline characters, then count the remaining characters
    filtered_chars = filter(c -> c != ' ' && c != '\n', title)
    return length(filtered_chars)
end
@assert count_composition_title_chars("234") == 3
@assert count_composition_title_chars("Ca 45") == 4
@assert count_composition_title_chars("Hello\nWorld") == 10
@assert count_composition_title_chars(" A B C ") == 3
@assert count_composition_title_chars("\n\n") == 0