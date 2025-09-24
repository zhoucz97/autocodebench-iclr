
--[[
Determine if a given string is a palindrome. A palindrome is a word, phrase, number, or other 
sequence of characters that reads the same forward and backward (ignoring spaces, 
punctuation, and capitalization).
    >>> isPalindrome("racecar") -- true
]]--

function isPalindrome(str)
    -- Remove all non-alphanumeric characters and convert to lowercase
    local cleaned = str:gsub("[^%a%d]", ""):lower()
    
    -- Compare the cleaned string with its reverse
    return cleaned == cleaned:reverse()
end
function testIsPalindrome()
    assert(isPalindrome("A man a plan a canal Panama") == true)
    assert(isPalindrome("No lemon, no melon") == true)
    assert(isPalindrome("Was it a car or a cat I saw") == true)
    assert(isPalindrome("Madam, in Eden, I'm Adam") == true)
    assert(isPalindrome("Never odd or even") == true)
    assert(isPalindrome("Eva, can I see bees in a cave") == true)
    assert(isPalindrome("hello") == false)
    assert(isPalindrome("GitHub") == false)
    assert(isPalindrome("programming") == false)
end

testIsPalindrome()