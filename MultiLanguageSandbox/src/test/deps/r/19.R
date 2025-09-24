

# This function, next_perfect_square, calculates the smallest perfect square number that is greater than or equal to a given integer n. A perfect square is a number that can be expressed as the square of an integer. 
# Examples:
# next_perfect_square(71711): In this case, the function returns 71824, as it is the smallest perfect square greater than 71711.


next_perfect_square <- function(n) {
  sqrt_n <- ceiling(sqrt(n))
  return(sqrt_n^2)
}
# Test cases
main <- function() {

    stopifnot(next_perfect_square(71711) == 71824)
    stopifnot(next_perfect_square(16) == 16)
    stopifnot(next_perfect_square(15) == 16)
    stopifnot(next_perfect_square(1) == 1)
    stopifnot(next_perfect_square(26) == 36)
    stopifnot(next_perfect_square(100000) == 100489)

}
# Run tests
main()