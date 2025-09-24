function quicksort(list)
    if list == nil or #list <= 1 then
        return list or {}
    end
    
    local pivot = list[1]
    local rest = {}
    for i = 2, #list do
        table.insert(rest, list[i])
    end
    
    local less = {}
    local greater = {}
    for _, x in ipairs(rest) do
        if x < pivot then
            table.insert(less, x)
        else
            table.insert(greater, x)
        end
    end
    
    local result = {}
    for _, v in ipairs(quicksort(less)) do
        table.insert(result, v)
    end
    table.insert(result, pivot)
    for _, v in ipairs(quicksort(greater)) do
        table.insert(result, v)
    end
    
    return result
end

function array_equal(a, b)
    if #a ~= #b then
        return false
    end
    for i = 1, #a do
        if a[i] ~= b[i] then
            return false
        end
    end
    return true
end

function check_quicksort()
    assert(array_equal(quicksort({3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5}), {1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9}))
    assert(array_equal(quicksort({5, 4, 3, 2, 1}), {1, 2, 3, 4, 5}))
    assert(array_equal(quicksort({}), {}))
    assert(array_equal(quicksort({1}), {1}))
end

check_quicksort()
