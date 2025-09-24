
--[[
Count the number of acute-angled triangles that can be formed by selecting any 3 edges out of a given set of edges.

Args:
- edges: A table containing integer values representing the lengths of the edges.

Returns:
int: The count of distinct acute-angled triangles that can be formed.

Examples:
>>> count_acute_triangles({1, 1, 1, 1})
4
]]

function count_acute_triangles(edges)
    local count = 0
    local n = #edges
    -- Sort the edges to facilitate processing
    table.sort(edges)
    
    -- Iterate through all possible triplets
    for i = 1, n - 2 do
        for j = i + 1, n - 1 do
            for k = j + 1, n do
                local a, b, c = edges[i], edges[j], edges[k]
                -- Check triangle inequality: a + b > c
                if a + b > c then
                    -- Check for acute triangle: a^2 + b^2 > c^2
                    if a * a + b * b > c * c then
                        count = count + 1
                    end
                end
            end
        end
    end
    
    return count
end
function testCountAcuteTriangles()
    assert(count_acute_triangles({1, 1, 1, 1}) == 4)
    assert(count_acute_triangles({1, 2, 3}) == 0)
    -- Additional tests to ensure correctness
    assert(count_acute_triangles({3, 4, 5, 7, 10}) == 0)
    assert(count_acute_triangles({6, 8, 10, 5, 5, 5}) == 4)
end

testCountAcuteTriangles()