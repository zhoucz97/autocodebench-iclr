

# This function calculates the number of ways a knight (马) in Chinese chess can traverse a given n x m chessboard, starting from position (x, y), without revisiting any point. The knight moves in an "L" shape, similar to the knight in international chess.
# The function takes four arguments: n (number of rows), m (number of columns), and the starting coordinates x and y (1-indexed). The function returns the total number of unique paths the knight can take to cover every point on the board exactly once.
# Examples:
# >>> knight_tour_paths(5, 4, 1, 1)
#     32
# >>> knight_tour_paths(3, 3, 1, 1)
#     0   

knight_tour_paths <- function(n, m, sta_x, sta_y) {
    moves <- list(
        c(-2, -1), c(-1, -2), c(1, -2), c(2, -1),
        c(2, 1), c(1, 2), c(-1, 2), c(-2, 1)
    )
    visited <- matrix(FALSE, nrow = n, ncol = m)
    count <- 0
    backtrack <- function(x, y, steps) {
        if (steps == n * m) {
            count <<- count + 1
            return()
        }
        
        for (move in moves) {
            dx <- move[1]
            dy <- move[2]
            nx <- x + dx
            ny <- y + dy
            
            if (nx >= 1 && nx <= n && ny >= 1 && ny <= m && !visited[nx, ny]) {
                visited[nx, ny] <- TRUE
                backtrack(nx, ny, steps + 1)
                visited[nx, ny] <- FALSE
            }
        }
    }
    visited[sta_x, sta_y] <- TRUE
    backtrack(sta_x, sta_y, 1)
    
    return(count)
}
# Test cases
main <- function() {

    stopifnot(knight_tour_paths(5, 4, 1, 1) == 32)
    stopifnot(knight_tour_paths(3, 3, 1, 1) == 0)
    stopifnot(knight_tour_paths(3, 4, 1, 1) == 2)


}


# Run tests
main()