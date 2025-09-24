function calculate_average_age(students_ages::Vector{Int}) :: Float64
    """
    Calculate the average age of a group of students. The ages are provided as a list of integers. Keep to two decimal places
    
    # Examples
    >>> calculate_average_age([18, 17, 20])
    18.33
    >>> calculate_average_age([16, 22, 19, 20])
    19.25
    """
    average = sum(students_ages) / length(students_ages)
    return round(average, digits=2)
end
@assert calculate_average_age([18, 17, 20]) ≈ 18.33
@assert calculate_average_age([16, 22, 19, 20]) ≈ 19.25
@assert calculate_average_age([15, 25, 16, 24]) ≈ 20.00
@assert calculate_average_age([21, 22, 23, 24, 25]) ≈ 23.00