
--[[
Given a sequence of n events, each occurring with a periodicity of a_i years,
the problem is to find the year Y_n when the n-th event occurs, under the condition
that the countdown for event i+1 can only commence in the year immediately following
the occurrence of event i.

    >>> apocalypseYear(6, {3, 2, 4, 5, 9, 18})
    36
]]--

function apocalypseYear(n, signs)
    if n == 0 then return 0 end
    local current_year = signs[1]
    for i = 2, n do
        local period = signs[i]
        -- The next event must occur in a year >= current_year + 1 and divisible by period
        local next_year = current_year + 1
        -- Find the smallest multiple of period >= next_year
        local remainder = next_year % period
        if remainder == 0 then
            current_year = next_year
        else
            current_year = next_year + (period - remainder)
        end
    end
    return current_year
end
function testApocalypseYear()
    assert(apocalypseYear(6, {3, 2, 4, 5, 9, 18}) == 36)
    assert(apocalypseYear(5, {1, 2, 3, 4, 5}) == 5)
    assert(apocalypseYear(5, {1, 1, 1, 1, 1}) == 5)
    assert(apocalypseYear(6, {50, 30, 711, 200, 503, 1006}) == 2012)
    assert(apocalypseYear(2, {1, 2}) == 2)
    assert(apocalypseYear(3, {3, 1, 2}) == 6)
    assert(apocalypseYear(3, {2, 3, 4}) == 4)
    assert(apocalypseYear(4, {1, 2, 3, 4}) == 4)
    assert(apocalypseYear(4, {5, 7, 11, 13}) == 13)
    assert(apocalypseYear(5, {2, 2, 2, 2, 2}) == 10)
    assert(apocalypseYear(3, {6, 10, 15}) == 15)
    assert(apocalypseYear(3, {4, 6, 14}) == 14)
    assert(apocalypseYear(4, {50, 30, 711, 200}) == 800)
    assert(apocalypseYear(6, {1, 1, 1, 1, 1, 1}) == 6)
    assert(apocalypseYear(2, {1000000, 999999}) == 1999998)
end

testApocalypseYear()