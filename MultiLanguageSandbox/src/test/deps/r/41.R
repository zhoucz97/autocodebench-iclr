


# This function find_comparable_opponents calculates the number of pairs of students who are considered "comparable opponents". Two students are "comparable opponents" if the difference in each of their subject scores (Math, English, Chinese) does not exceed 5 points and the total score difference does not exceed 10 points.
# The function takes a matrix as input where each row represents a student and each column represents their scores in Chinese, Math, and English, respectively.
# Examples:
# >>> find_comparable_opponents(matrix(c(90, 90, 90, 85, 95, 90, 80, 100, 91), nrow = 3, byrow = TRUE))
#     2
find_comparable_opponents <- function(student_scores) {
  n <- nrow(student_scores)
  count <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      diff_chinese <- abs(student_scores[i, 1] - student_scores[j, 1])
      diff_math <- abs(student_scores[i, 2] - student_scores[j, 2])
      diff_english <- abs(student_scores[i, 3] - student_scores[j, 3])
      if (diff_chinese <= 5 && diff_math <= 5 && diff_english <= 5) {
        total_diff <- abs(sum(student_scores[i, ]) - sum(student_scores[j, ]))
        if (total_diff <= 10) {
          count <- count + 1
        }
      }
    }
  }
  
  return(count)
}
# Test cases
main <- function() {
    # Test case 1
    stopifnot(find_comparable_opponents(matrix(c(90, 90, 90, 85, 95, 90, 80, 100, 91), nrow = 3, byrow = TRUE)) == 2)

    # Test case 2
    stopifnot(find_comparable_opponents(matrix(c(100, 100, 100, 95, 95, 95, 90, 90, 90), nrow = 3, byrow = TRUE)) == 0)

    # Test case 3
    stopifnot(find_comparable_opponents(matrix(c(50, 60, 70, 55, 65, 70, 80, 80, 80), nrow = 3, byrow = TRUE)) == 1)



}


# Run tests
main()