

# This function max_path_sum calculates the maximum sum of numbers along a path from the top to the bottom of a triangle represented by a matrix. Each step can only move to the number directly below or to the number to the immediate right below the current number. The input is a matrix where each row represents a level of the triangle.

# Examples:

# # Example 1
# triangle_matrix <- matrix(c(7, NA, NA, NA,
#                             3, 8, NA, NA,
#                             8, 1, 0, NA,
#                             2, 7, 4, 4),
#                           nrow = 4, byrow = TRUE)
# max_path_sum(triangle_matrix)
# # Output: 25

# # Example 2
# triangle_matrix <- matrix(c(7, NA, NA,
#                             3, 8, NA,
#                             8, 1, 0),
#                           nrow = 3, byrow = TRUE)
# max_path_sum(triangle_matrix)
# # Output: 18


max_path_sum <- function(triangle_matrix) {
    n <- nrow(triangle_matrix)
    dp <- matrix(0, nrow = n, ncol = n)
    for (j in 1:n) {
        if (!is.na(triangle_matrix[n, j])) {
            dp[n, j] <- triangle_matrix[n, j]
        } else {
            dp[n, j] <- 0
        }
    }
    for (i in (n-1):1) {
        for (j in 1:i) {
            if (!is.na(triangle_matrix[i, j])) {
                dp[i, j] <- triangle_matrix[i, j] + max(dp[i+1, j], dp[i+1, j+1])
            } else {
                dp[i, j] <- 0
            }
        }
    }
    
    return(dp[1, 1])
}
# Test cases
main <- function() {


triangle_matrix <- matrix(c(7, NA, NA, NA, NA,
                            3, 8, NA, NA, NA,
                            8, 1, 0, NA, NA,
                            2, 7, 4, 4, NA, 
                            4, 5, 2, 6, 5),
                          nrow = 5, byrow = TRUE)
stopifnot(max_path_sum(triangle_matrix) == 30)

# Test case 1
triangle_matrix <- matrix(c(7, NA, NA, NA,
                            3, 8, NA, NA,
                            8, 1, 0, NA,
                            2, 7, 4, 4),
                          nrow = 4, byrow = TRUE)
stopifnot(max_path_sum(triangle_matrix) == 25)

# Test case 2
triangle_matrix <- matrix(c(7, NA, NA,
                            3, 8, NA,
                            8, 1, 0),
                          nrow = 3, byrow = TRUE)
stopifnot(max_path_sum(triangle_matrix) == 18)

# Test case 3
triangle_matrix <- matrix(c(1, NA, NA, NA,
                            2, 3, NA, NA,
                            4, 5, 6, NA,
                            7, 8, 9, 10),
                          nrow = 4, byrow = TRUE)
stopifnot(max_path_sum(triangle_matrix) == 20)


}


# Run tests
main()