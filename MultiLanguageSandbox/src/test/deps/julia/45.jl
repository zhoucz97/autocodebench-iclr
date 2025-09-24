function max_product_partition(n::Int, m::Int) :: Vector{Int}
    """
    Find `m` positive integers whose sum is `n`, and their product is as large as possible. Return the partition in lexicographically smallest order.

    # Examples
    - `max_product_partition(6, 3)` should return `[2, 2, 2]` because 2*2*2 is the largest product you can get from three numbers that add up to 6.
    - `max_product_partition(8, 3)` should return `[2, 3, 3]` since 2*3*3 = 18 is the largest product from three numbers that add up to 8.

    # Arguments
    - `n`: The sum of the numbers.
    - `m`: The number of positive integers.

    # Returns
    - A vector of integers representing the partition.
    """
    if m <= 0 || n < m
        error("Invalid input: m must be positive and n must be at least m")
    end
    
    base = n ÷ m
    remainder = n % m
    
    # Create a vector with (m - remainder) elements of base and remainder elements of base + 1
    partition = vcat(fill(base, m - remainder), fill(base + 1, remainder))
    
    # Sort to ensure lexicographical order (though it's already sorted in this approach)
    sort!(partition)
    
    return partition
end
@assert max_product_partition(6, 3) == [2, 2, 2]
@assert max_product_partition(8, 3) == [2, 3, 3]
@assert max_product_partition(10, 2) == [5, 5]
@assert max_product_partition(7, 3) == [2, 2, 3]