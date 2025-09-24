function count_climb_ways(steps::Int) :: Int
    """
    Calculate the number of distinct ways to climb a given number of stairs when
    each step can be either 1 or 2 stairs.
    
    # Examples
    - `count_climb_ways(2)` should return `2`, representing [1 step + 1 step, 2 steps].
    - `count_climb_ways(4)` should return `5`, representing [1+1+1+1, 1+1+2, 1+2+1, 2+1+1, 2+2].
    - `count_climb_ways(3)` should return `3`, representing [1+1+1, 1+2, 2+1].
    """
    if steps == 0
        return 1
    elseif steps == 1
        return 1
    end
    
    a, b = 1, 1  # Represents ways(0) and ways(1)
    for _ in 2:steps
        a, b = b, a + b
    end
    return b
end
@assert count_climb_ways(2) == 2
@assert count_climb_ways(3) == 3
@assert count_climb_ways(4) == 5
@assert count_climb_ways(5) == 8