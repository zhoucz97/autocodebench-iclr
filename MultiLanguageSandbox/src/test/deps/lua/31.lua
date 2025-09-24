
--[[
Given a string consisting of lowercase English letters, in each round you
can change one of the characters to another character. The question is: what
is the minimum number of rounds needed to make the string composed of the 
same character?

Examples:
    -- minRoundsToSameChar("aab") should return 1
--]]

function minRoundsToSameChar(s)
    local charCounts = {}
    for i = 1, #s do
        local char = s:sub(i, i)
        charCounts[char] = (charCounts[char] or 0) + 1
    end
    
    local maxCount = 0
    for _, count in pairs(charCounts) do
        if count > maxCount then
            maxCount = count
        end
    end
    
    return #s - maxCount
end
function testMinRoundsToSameChar()
    assert(minRoundsToSameChar("aab") == 1)
    assert(minRoundsToSameChar("abc") == 2)
    assert(minRoundsToSameChar("aaa") == 0)
    assert(minRoundsToSameChar("abab") == 1)
    assert(minRoundsToSameChar("zzzzz") == 0)
end

testMinRoundsToSameChar()