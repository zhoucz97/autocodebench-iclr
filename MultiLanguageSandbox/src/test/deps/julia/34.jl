function calculate_highest_scholarship(students::Vector{Tuple{String, Int, Int, Char, Char, Int}}) :: Tuple{String, Int}
    """
    Calculate the student(s) who receive the highest total scholarship amount among a group of students based on specific criteria.

    Parameters:
    - students (Vector{Tuple{String, Int, Int, Char, Char, Int}}): A vector of tuples representing students' data. Each tuple contains:
    - Student's name (String)
    - Final average score (Int)
    - Class rating score (Int)
    - Whether the student is a student cadre (Char: 'Y' for yes, 'N' for no)
    - Whether the student is from a western province (Char: 'Y' for yes, 'N' for no)
    - Number of papers published (Int)

    Returns:
    - Tuple{String, Int}: A tuple containing the name of the student(s) with the highest total scholarship amount and the total scholarship amount received.

    Example:
    ```julia
    students_data = [("YaoLin", 87, 82, 'Y', 'N', 0),
                    ("ChenRuiyi", 88, 78, 'N', 'Y', 1),
                    ("LiXin", 92, 88, 'N', 'N', 0),
                    ("ZhangQin", 83, 87, 'Y', 'N', 1)]

    calculate_highest_scholarship(students_data)
    # Output: ("ChenRuiyi", 9000)
    """
    max_scholarship = -1
    top_student = ""
    
    for student in students
        name, avg_score, class_rating, cadre, western, papers = student
        
        # Calculate base scholarship
        if avg_score >= 90
            base = 5000
        elseif avg_score >= 80
            base = 3000
        else
            base = 1000
        end
        
        # Calculate additional scholarships
        total = base
        if cadre == 'Y'
            total += 1000
        end
        if western == 'Y'
            total += 1500
        end
        total += papers * 500
        
        # Update max scholarship and top student
        if total > max_scholarship
            max_scholarship = total
            top_student = name
        end
    end
    
    return (top_student, max_scholarship)
end
students_data = [("YaoLin", 87, 82, 'Y', 'N', 0),
                 ("ChenRuiyi", 88, 78, 'N', 'Y', 1),
                 ("LiXin", 92, 88, 'N', 'N', 0),
                 ("ZhangQin", 83, 87, 'Y', 'N', 1)]

@assert calculate_highest_scholarship(students_data) == ("ChenRuiyi", 9000)

students_data = [("YaoLin", 87, 82, 'Y', 'N', 0),
                 ("LiXin", 92, 88, 'N', 'N', 0),
                 ("ZhangQin", 83, 87, 'Y', 'N', 1)]
@assert calculate_highest_scholarship(students_data) == ("ZhangQin", 8850)