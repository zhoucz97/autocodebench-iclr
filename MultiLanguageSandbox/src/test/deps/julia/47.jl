function count_common_students(a::Vector{Int}, b::Vector{Int}) :: Int
    """
    Counts the number of students who have enrolled in both course A and course B.

    # Arguments
    - `a::Vector{Int}`: A vector of student IDs enrolled in course A.
    - `b::Vector{Int}`: A vector of student IDs enrolled in course B.

    # Returns
    - `Int`: The count of students enrolled in both courses.

    # Examples
    ```jldoctest
    julia> count_common_students([1, 2, 3, 4, 5], [1, 3, 4, 5, 6])
    4

    julia> count_common_students([10, 11, 12], [13, 14, 15])
    0
    """
    # Convert both vectors to sets to find the intersection
    set_a = Set(a)
    set_b = Set(b)
    
    # Find the intersection of the two sets
    common_students = intersect(set_a, set_b)
    
    # Return the count of common students
    return length(common_students)
end
@assert count_common_students([1, 2, 3, 4, 5], [1, 3, 4, 5, 6]) == 4
@assert count_common_students([10, 11, 12], [13, 14, 15]) == 0
@assert count_common_students([1, 2, 3], [3, 4, 5, 6]) == 1
@assert count_common_students([7, 8, 9, 10], [10, 11, 12, 7]) == 2