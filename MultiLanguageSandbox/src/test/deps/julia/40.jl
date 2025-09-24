function count_dictionary_lookups(memory_capacity::Int, words::Vector{Int}) :: Int
    """
    Count the number of times a translation software has to look up words in an external dictionary while translating an article.

    The software has a memory that can store a limited number of word translations. If a word's translation is not in memory, it looks it up in the external dictionary and stores it in memory. If the memory is full, the oldest entry is removed to make space for the new one.

    Parameters:
    - memory_capacity: An integer representing the number of translations that can be stored in memory at a time.
    - article_length: An integer representing the number of words in the article.
    - words: A vector of integers, each representing a unique word in the article.

    Returns:
    - An integer representing the number of dictionary lookups performed.

    Examples:
    >>> count_dictionary_lookups(3, [1, 2, 1, 5, 4, 4, 1])
    5
    >>> count_dictionary_lookups(2, [1, 2, 3, 2, 1])
    4
    """
    memory = Set{Int}()
    queue = Vector{Int}()
    lookups = 0
    
    for word in words
        if !(word in memory)
            lookups += 1
            push!(queue, word)
            push!(memory, word)
            if length(queue) > memory_capacity
                oldest = popfirst!(queue)
                delete!(memory, oldest)
            end
        end
    end
    
    return lookups
end
@assert count_dictionary_lookups(3, [1, 2, 1, 5, 4, 4, 1]) == 5
@assert count_dictionary_lookups(2, [1, 2, 3, 2, 1]) == 4
@assert count_dictionary_lookups(1, [1, 2, 1, 2, 1]) == 5
@assert count_dictionary_lookups(4, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) == 10