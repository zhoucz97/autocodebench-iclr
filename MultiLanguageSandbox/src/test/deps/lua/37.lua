
--[[
Given a sequence of student IDs entering a classroom, where the IDs range from 0 to N-1, calculate the total number of 
handshakes that occur. Each student will shake hands with every student already in the classroom with a smaller ID number.
The sequence represents the order in which students enter the classroom.

Examples:
    >>> countHandshakes(3, {2, 1, 0})
    0
]]--

function countHandshakes(n, order)
    local present = {}
    local total = 0
    for _, id in ipairs(order) do
        -- Count how many students in 'present' have IDs less than 'id'
        local count = 0
        for _, pid in ipairs(present) do
            if pid < id then
                count = count + 1
            end
        end
        total = total + count
        table.insert(present, id)
    end
    return total
end
function testCountHandshakes()
    assert(countHandshakes(4, {2, 1, 3, 0}) == 2)
    assert(countHandshakes(6, {0, 1, 2, 3, 4, 5}) == 15)
    assert(countHandshakes(3, {1, 2, 0}) == 1)

    assert(countHandshakes(4, {3, 2, 1, 0}) == 0)
    assert(countHandshakes(4, {0, 1, 2, 3}) == 6)
    assert(countHandshakes(6, {5, 4, 3, 2, 1, 0}) == 0)

    assert(countHandshakes(4, {0, 2, 1, 3}) == 5)
    assert(countHandshakes(5, {3, 1, 4, 2, 0}) == 3)
    assert(countHandshakes(4, {1, 0, 3, 2}) == 4)

    assert(countHandshakes(3, {2, 0, 1}) == 1)
    assert(countHandshakes(5, {1, 3, 0, 2, 4}) == 7)
    assert(countHandshakes(5, {4, 3, 2, 1, 0}) == 0)
end

testCountHandshakes()