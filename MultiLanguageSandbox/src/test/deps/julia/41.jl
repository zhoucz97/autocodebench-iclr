function count_competitive_pairs(scores::Vector{Vector{Int64}}) :: Int
    """
    Determine the number of pairs of students who are considered "competitive pairs" based on their scores.
    A pair is considered competitive if the absolute difference in each subject (Math, English, Science) is <= 5,
    and the total score difference is <= 10.
    - scores: A Nx3 matrix where each row represents a student and columns represent scores in Math, English, and Science.

    # Examples
    >>> count_competitive_pairs([[90, 90, 90], [85, 95, 90], [80, 100, 91]])
    2
    """
    n = length(scores)
    count = 0
    
    for i in 1:n-1
        for j in i+1:n
            # Calculate absolute differences in each subject
            math_diff = abs(scores[i][1] - scores[j][1])
            english_diff = abs(scores[i][2] - scores[j][2])
            science_diff = abs(scores[i][3] - scores[j][3])
            
            # Check if all subject differences are <= 5
            if math_diff <= 5 && english_diff <= 5 && science_diff <= 5
                # Calculate total score difference
                total_diff = abs(sum(scores[i]) - sum(scores[j]))
                
                # Check if total difference is <= 10
                if total_diff <= 10
                    count += 1
                end
            end
        end
    end
    
    return count
end
@assert count_competitive_pairs([[90, 90, 90], [85, 95, 90], [80, 100, 91]]) == 2
@assert count_competitive_pairs([[100, 100, 100], [95, 95, 95], [90, 90, 90]]) == 0
@assert count_competitive_pairs([[100, 105, 100], [95, 100, 95], [90, 85, 90], [85, 80, 85]]) == 0
@assert count_competitive_pairs([[110, 115, 120], [110, 110, 115], [100, 105, 110], [95, 100, 105]]) == 1