


# The function `minimum_cost_path` calculates the minimum cost needed for a merchant to traverse from the top-left corner to the bottom-right corner of an N×N square grid within (2N-1) time units, where each cell of the grid has a specific cost associated with passing through it. The merchant can only move left, right, up, or down, but cannot move diagonally or leave the grid.
# The function accepts a matrix 'grid' where each element represents the cost of traversing that cell.
# Examples:
# >>> minimum_cost_path(matrix(c(1, 2, 6, 10, 20, 4, 5, 8, 11, 23, 6, 7, 9, 12, 25, 8, 15, 18, 19, 29, 10, 17, 20, 21, 33), nrow = 5, ncol = 5, byrow = TRUE))
#     109


minimum_cost_path <- function(grid) {
    n <- nrow(grid)
    total_steps <- 2 * n - 1

    prev_step <- matrix(Inf, nrow = n, ncol = n)
    prev_step[1, 1] <- grid[1, 1]
    
    for (step in 1:total_steps) {
        current_step <- matrix(Inf, nrow = n, ncol = n)
        for (i in 1:n) {
            for (j in 1:n) {
                if (i == 1 && j == 1) {
                    if (step == 0) {
                        current_step[i, j] <- grid[i, j]
                    }
                    next
                }
                min_prev <- Inf
                if (i > 1) {
                    if (prev_step[i-1, j] < min_prev) {
                        min_prev <- prev_step[i-1, j]
                    }
                }
                if (i < n) {
                    if (prev_step[i+1, j] < min_prev) {
                        min_prev <- prev_step[i+1, j]
                    }
                }
                if (j > 1) {
                    if (prev_step[i, j-1] < min_prev) {
                        min_prev <- prev_step[i, j-1]
                    }
                }
                if (j < n) {
                    if (prev_step[i, j+1] < min_prev) {
                        min_prev <- prev_step[i, j+1]
                    }
                }
                if (min_prev != Inf) {
                    current_step[i, j] <- min_prev + grid[i, j]
                }
            }
        }
        prev_step <- current_step
    }
    
    return(prev_step[n, n])
}
# Test cases
main <- function() {

  stopifnot(minimum_cost_path(matrix(c(1, 2, 6, 10, 20, 4, 5, 8, 11, 23, 6, 7, 9, 12, 25, 8, 15, 18, 19, 29, 10, 17, 20, 21, 33), nrow = 5, ncol = 5, byrow = TRUE)) == 109)

  stopifnot(minimum_cost_path(matrix(c(1, 2, 6, 10), nrow = 2, ncol = 2, byrow = TRUE)) == 13)
}


# Run tests
main()