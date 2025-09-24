


# This function 'calculate_game_points' calculates the total points scored in a series of game rounds. In each round, a player is given n integers. The player scores a point in a round if they can select two numbers from these integers whose sum equals k. The function takes the total number of rounds n, the target sum k, and a list of vectors, where each vector represents the integers given in each round.
# Examples:
# calculate_game_points(3, 20, list(c(1, 3, 5), c(9, 15, 5), c(1, 10, 10))): This returns 2, as in two of the rounds, the player can find pairs that sum up to 20.


calculate_game_points <- function(n, k, game_rounds) {
  total_points <- 0
  
  for (round in game_rounds) {
    found <- FALSE
    for (i in 1:(length(round) - 1)) {
      for (j in (i + 1):length(round)) {
        if (round[i] + round[j] == k) {
          found <- TRUE
          break
        }
      }
      if (found) break
    }
    if (found) {
      total_points <- total_points + 1
    }
  }
  
  return(total_points)
}
# Test cases
main <- function() {
    stopifnot(calculate_game_points(3, 20, list(c(1, 3, 5), c(9, 15, 5), c(1, 10, 10))) == 2)
    stopifnot(calculate_game_points(2, 10, list(c(2, 5, 3), c(1, 9, 4))) == 1)
    stopifnot(calculate_game_points(4, 15, list(c(4, 11, 7), c(2, 6, 9), c(8, 5, 2), c(3, 12, 6))) == 3)
    stopifnot(calculate_game_points(3, 8, list(c(1, 2, 5), c(3, 4, 1), c(6, 2, 0))) == 1)
    stopifnot(calculate_game_points(2, 5, list(c(1, 1, 1), c(2, 2, 1))) == 0)
}

# Run tests
main()