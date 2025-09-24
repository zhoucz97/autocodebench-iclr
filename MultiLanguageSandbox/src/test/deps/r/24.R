


# This function longest_increasing_subsequence takes a vector sequence representing a sequence of integers and returns the length of the longest increasing subsequence within it. An increasing subsequence is defined as a set of numbers in the sequence that are in increasing order and are taken from the original sequence without changing their order. The length of the subsequence is the number of elements it contains.

# Example 1:
# Input: c(1, 7, 3, 5, 9, 4, 8)
# Output: 4 (The longest increasing subsequence is 1, 3, 5, 8)

# Example 2:
# Input: c(10, 22, 9, 33, 21, 50, 41, 60)
# Output: 5 (The longest increasing subsequence is 10, 22, 33, 50, 60)

longest_increasing_subsequence <- function(sequence) {
  n <- length(sequence)
  if (n == 0) return(0)

  dp <- rep(1, n)
  
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      if (sequence[i] > sequence[j] && dp[i] < dp[j] + 1) {
        dp[i] <- dp[j] + 1
      }
    }
  }
  return(max(dp))
}
# Test cases
main <- function() {
    stopifnot(longest_increasing_subsequence(c(1, 7, 3, 5, 9, 4, 8)) == 4)
    stopifnot(longest_increasing_subsequence(c(10, 22, 9, 33, 21, 50, 41, 60)) == 5)
    stopifnot(longest_increasing_subsequence(c(5, 3, 4, 8, 6, 7)) == 4)

}


# Run tests
main()