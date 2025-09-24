


# This function modular_exponentiation computes the modular exponentiation of two numbers. It calculates (A^B) mod P, where A and B are non-negative integers and P is a non-negative integer representing the modulus. This function returns the result of the modular exponentiation.

# Examples:
# modular_exponentiation(2, 5, 3): In this case, the function returns 2.
# modular_exponentiation(3, 4, 5): Here, the function returns 1.

modular_exponentiation <- function(A, B, P) {
  if (P == 1) return(0)
  
  result <- 1
  A <- A %% P
  
  while (B > 0) {
    if (B %% 2 == 1) {
      result <- (result * A) %% P
    }
    A <- (A * A) %% P
    B <- B %/% 2
  }
  
  return(result)
}
# Test cases
main <- function() {
    stopifnot(modular_exponentiation(2, 5, 3) == 2)
    stopifnot(modular_exponentiation(3, 4, 5) == 1)
    stopifnot(modular_exponentiation(7, 3, 13) == 5)
    stopifnot(modular_exponentiation(10, 11, 19) == 14)
    stopifnot(modular_exponentiation(17, 13, 31) == 3)
}

# Run tests
main()