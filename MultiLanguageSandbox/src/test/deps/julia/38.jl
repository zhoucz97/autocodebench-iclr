function calculate_watering_time(student_water_needs::Vector{Int}, faucets::Int) :: Int
    """
    Calculates the total time required for a group of students to fill their water containers using a limited number of faucets.

    Each faucet delivers water at a constant rate of 1 unit per second. Students are queued in a given order and each has a specific amount of water they need to collect. Students fill their containers simultaneously using the available faucets. As soon as a student finishes, the next in line begins to fill their container without delay. If there are more faucets than students, only the needed number of faucets are used.

    # Arguments
    - `student_water_needs`: A list of integers representing the amount of water each student needs.
    - `faucets`: The number of available faucets.

    # Examples
    >>> calculate_watering_time([4, 4, 1, 2, 1], 3)
    4
    >>> calculate_watering_time([2, 3, 1], 2)
    3
    """
    # Initialize a min-heap to keep track of the end times of the faucets
    heap = BinaryMinHeap{Int}()
    
    # Initialize the heap with the number of faucets, all starting at time 0
    for _ in 1:min(faucets, length(student_water_needs))
        push!(heap, 0)
    end
    
    # Iterate through the student water needs
    for need in student_water_needs
        # Get the earliest available faucet
        earliest_end_time = pop!(heap)
        
        # Assign the student to this faucet and update the end time
        new_end_time = earliest_end_time + need
        
        # Push the new end time back into the heap
        push!(heap, new_end_time)
    end
    
    # The total time is the maximum end time among all faucets
    return maximum(heap)
end

# Test cases
@assert calculate_watering_time([4, 4, 1, 2, 1], 3) == 4
@assert calculate_watering_time([2, 3, 1], 2) == 3
@assert calculate_watering_time([5, 3, 2, 1], 1) == 11
@assert calculate_watering_time([1, 1, 1, 1, 1], 5) == 1