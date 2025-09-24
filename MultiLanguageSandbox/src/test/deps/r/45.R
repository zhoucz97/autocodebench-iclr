

# This function max_product_distribution finds M positive integers that sum up to N, with the objective of maximizing their product. The function returns a vector of these integers, arranged in non-decreasing order (to ensure the lexicographically smallest sequence if multiple solutions exist).

# Examples:
# max_product_distribution(6, 3) should return c(2, 2, 2), because 2 * 2 * 2 = 8 is the maximum product achievable with three numbers that sum to 6.
# max_product_distribution(8, 3) should return c(2, 3, 3), because 2 * 3 * 3 = 18 is the maximum product achievable with three numbers that sum to 8.

max_product_distribution <- function(N, M) {
  if (M == 1) {
    return(c(N))
  }


  res <- numeric(M)
  base <- floor(N / M)
  remainder <- N %% M
  res[1:remainder] <- base + 1
  res[(remainder + 1):M] <- base











  res <- rep(1, M)
  remaining <- N - M

  i <- 1
  while (remaining > 0) {
    if (i > M) {
      i <- 1
    }
    if (res[i] < 3) {
      res[i] <- res[i] + 1
      remaining <- remaining - 1
    } else {
      i <- i + 1
    }
  }





  sort(res)
}
# Test cases
main <- function() {

    stopifnot(all.equal(max_product_distribution(6, 3), c(2, 2, 2)))
    stopifnot(all.equal(max_product_distribution(8, 3), c(2, 3, 3)))
    stopifnot(all.equal(max_product_distribution(7, 2), c(3, 4)))


}


# Run tests
main()