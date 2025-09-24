function bacteria_distribution(initial_count::Int, days::Int) :: Array{Int, 2}
    """
    Compute the distribution of bacteria in a 9x9 petri dish over a specified number of days. Initially, a given number of bacteria are placed in the center of the dish. Suppose a bacterium lives only one day. Each day, each bacterium produces 10 offspring. Two of these remain in the original cell, while the other eight are evenly distributed among the adjacent cells.

    # Examples
    - `bacteria_distribution(2, 1)` results in a 9x9 matrix with certain cells having bacteria counts.
    - `bacteria_distribution(3, 2)` will show the spread of bacteria after 2 days starting from 3 bacteria.

    The function returns a 9x9 matrix representing the bacteria distribution in the dish.
    """
    # Initialize a 9x9 matrix with zeros
    dish = zeros(Int, 9, 9)
    
    # Place the initial bacteria in the center of the dish
    center = 5  # Since indexing starts at 1, the center is at (5,5)
    dish[center, center] = initial_count
    
    # Directions for adjacent cells (up, down, left, right)
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    
    for _ in 1:days
        new_dish = zeros(Int, 9, 9)
        
        for i in 1:9
            for j in 1:9
                if dish[i, j] > 0
                    # Each bacterium produces 10 offspring
                    total_offspring = dish[i, j] * 10
                    
                    # Two stay in the same cell
                    new_dish[i, j] += 2 * dish[i, j]
                    
                    # Distribute the remaining eight among adjacent cells
                    remaining = total_offspring - 2 * dish[i, j]
                    for (di, dj) in directions
                        ni, nj = i + di, j + dj
                        if 1 <= ni <= 9 && 1 <= nj <= 9
                            new_dish[ni, nj] += remaining ÷ 4
                        end
                    end
                end
            end
        end
        
        # Update the dish for the next day
        dish = new_dish
    end
    
    return dish
end

# Example usage:
@assert bacteria_distribution(2, 1) == [
    0 0 0 0 0 0 0 0 0;
    0 0 0 0 0 0 0 0 0;
    0 0 0 0 0 0 0 0 0;
    0 0 0 2 2 2 0 0 0;
    0 0 0 2 4 2 0 0 0;
    0 0 0 2 2 2 0 0 0;
    0 0 0 0 0 0 0 0 0;
    0 0 0 0 0 0 0 0 0;
    0 0 0 0 0 0 0 0 0
]

@assert bacteria_distribution(2, 4) == [2 8 20 32 38 32 20 8 2; 8 40 104 176 208 176 104 40 8; 20 104 284 488 584 488 284 104 20; 32 176 488 856 1024 856 488 176 32; 38 208 584 1024 1232 1024 584 208 38; 32 176 488 856 1024 856 488 176 32; 20 104 284 488 584 488 284 104 20; 8 40 104 176 208 176 104 40 8; 2 8 20 32 38 32 20 8 2]