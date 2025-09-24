


# This function count_common_students calculates the number of students who have enrolled in both courses A and B.
# Given two vectors: course_a representing the student IDs enrolled in course A, and course_b representing the student IDs enrolled in course B,
# the function returns the count of students who are enrolled in both courses.
# The student IDs in each course are unique.
# Examples:
# >>> count_common_students(c(1, 2, 3, 4, 5), c(1, 3, 4, 5, 6))
#     4
# >>> count_common_students(c(2, 4, 6, 8), c(1, 3, 5, 7))
#     0
count_common_students <- function(course_a, course_b) {
  common_students <- intersect(course_a, course_b)
  return(length(common_students))
}
# Test cases
main <- function() {
    stopifnot(count_common_students(c(1, 2, 3, 4, 5), c(1, 3, 4, 5, 6)) == 4)
    stopifnot(count_common_students(c(2, 4, 6, 8), c(1, 3, 5, 7)) == 0)
    stopifnot(count_common_students(c(10, 12, 14, 16), c(14, 16, 18, 20)) == 2)
}

# Run tests
main()