
--[[
Creates an ID by combining two words in a specific manner.
Parameters:
- word1 (string): The first word to be used in the ID.
- word2 (string): The second word to be used in the ID.
Returns:
string: A divine ID formed by reversing every other character of the second 
        word and combining it with the first word.
e.g.:
    >>> create_id("fish","cat")
    "ftiasch"
]]

function create_id(word1, word2)
    -- Reverse every other character in word2
    local reversed_every_other = ""
    for i = 1, #word2 do
        if i % 2 == 0 then
            reversed_every_other = word2:sub(i, i) .. reversed_every_other
        else
            reversed_every_other = reversed_every_other .. word2:sub(i, i)
        end
    end
    
    -- Combine word1 and the modified word2
    return word1 .. reversed_every_other
end
function test_create_id()
    assert(create_id("fish", "cat") == "ftiasch")
    assert(create_id("icpc", "acm") == "imccpac")
    assert(create_id("oo", "w") == "owo")
    assert(create_id("hello", "world") == "hdellrloow")
    assert(create_id("abc", "def") == "afbecd")
    assert(create_id("buaanb", "nbbuaa") == "bauaauabnbbn")
    assert(create_id("xtuisgood", "ilovextu") == "xuttuxiesvgooloid")
end

test_create_id()