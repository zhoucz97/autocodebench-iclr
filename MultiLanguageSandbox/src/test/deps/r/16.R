

# This function `maximize_shortest_hop` is designed to determine the maximum possible minimum distance for a series of hops across a river, given a set of stones and the option to remove some of them. The river has stones placed at various distances from the start, and the goal is to ensure that the smallest hop distance between any two stones (including start and end) is as large as possible after removing up to M stones.
# Args:
#   L: The total length of the river (an integer).
#   N: The number of stones in the river (an integer).
#   M: The maximum number of stones that can be removed (an integer).
#   distances: A vector of integers representing the distances of each stone from the start.
# Returns:
#   An integer representing the longest possible minimum distance between any two stones after removing up to M stones.
# Examples:
#   maximize_shortest_hop(25, 5, 2, c(2, 11, 14, 17, 21)): In this case, the function should return 9.

maximize_shortest_hop <- function(L, N, M, distances) {
  distances <- sort(distances)
  left <- 1
  right <- L
  result <- 0
  
  while (left <= right) {
    mid <- floor((left + right) / 2)
    if (is_possible(distances, L, mid, M)) {
      result <- mid
      left <- mid + 1
    } else {
      right <- mid - 1
    }
  }
  
  return(result)
}
is_possible <- function(distances, L, target, M) {
  count <- 0
  prev <- 0
  
  for (i in 1:length(distances)) {
    if (distances[i] - prev < target) {
      count <- count + 1
    } else {
      prev <- distances[i]
    }
  }
  if (L - prev < target) {
    count <- count + 1
  }
  
  return(count <= M)
}
# Test cases
main <- function() {
    stopifnot(maximize_shortest_hop(25, 5, 2, c(2, 11, 14, 17, 21)) == 4)
    stopifnot(maximize_shortest_hop(100, 10, 3, c(5, 15, 20, 30, 40, 50, 60, 70, 80, 90)) == 10)
    stopifnot(maximize_shortest_hop(50, 4, 1, c(10, 20, 30, 40)) == 10)
    stopifnot(maximize_shortest_hop(30, 3, 1, c(5, 15, 25)) == 5)
    stopifnot(maximize_shortest_hop(200, 8, 3, c(20, 40, 60, 80, 120, 140, 160, 180)) == 20)
}

# Run tests
main()