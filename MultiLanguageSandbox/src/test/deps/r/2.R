


# This function finds the top 'm' largest elements from a given list of unique integers.
# The integers in the list should be within the range [-500000, 500000].
# Args:
#   numbers: A vector of integers.
#   m: An integer indicating the number of top elements to retrieve.
# Returns:
#   A vector of top 'm' largest elements sorted in descending order.
# Examples:
#   >>> top_n_elements(c(10, 3, 5, 20, -4), 2)
#   [1] 20 10
#   >>> top_n_elements(c(-1, -3, -2, -5, -4), 3)
#   [1] -1 -2 -3

top_n_elements <- function(numbers, m) {
  if (m > length(numbers)) {
    stop("m cannot be greater than the length of the input vector.")
  }
  sorted_numbers <- sort(numbers, decreasing = TRUE)
  top_m <- sorted_numbers[1:m]
  
  return(top_m)
}
# Test cases
main <- function() {
  stopifnot(identical(top_n_elements(c(10, 3, 5, 20, -4), 2), c(20, 10)))
  stopifnot(identical(top_n_elements(c(-1, -3, -2, -5, -4), 3), c(-1, -2, -3)))
  stopifnot(identical(top_n_elements(c(100, 200, 300, 400, 500), 3), c(500, 400, 300)))
  stopifnot(identical(top_n_elements(c(0, -100, -200, -300, -400), 4), c(0, -100, -200, -300)))
  stopifnot(identical(top_n_elements(c(1500, 1000, 500, -1000, -1500), 5), c(1500, 1000, 500, -1000, -1500)))

}

# Run tests
main()