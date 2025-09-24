

# This function longest_common_subsequence computes the length of the longest common subsequence between two strings. A subsequence is a sequence that can be derived from another sequence by deleting some or no elements without changing the order of the remaining elements.
# Examples:
# longest_common_subsequence("abcde", "ace"): In this case, the function returns 3 (the longest common subsequence is "ace").
# longest_common_subsequence("abc", "abc"): In this case, the function returns 3.
# longest_common_subsequence("abc", "def"): In this case, the function returns 0 (no common subsequence).



longest_common_subsequence <- function(string1, string2) {
  s1 <- unlist(strsplit(string1, ""))
  s2 <- unlist(strsplit(string2, ""))
  m <- length(s1)
  n <- length(s2)

  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      if (i == 0 || j == 0) {
        dp[i, j] <- 0
      } else if (s1[i - 1] == s2[j - 1]) {
        dp[i, j] <- dp[i - 1, j - 1] + 1
      } else {
        dp[i, j] <- max(dp[i - 1, j], dp[i, j - 1])
      }
    }
  }
  return(dp[m + 1, n + 1])
}
# Test cases
main <- function() {

    stopifnot(longest_common_subsequence("abcde", "ace") == 3)
    stopifnot(longest_common_subsequence("abc", "abc") == 3)
    stopifnot(longest_common_subsequence("abc", "def") == 0)
    stopifnot(longest_common_subsequence("abcdgh", "aedfhb") == 3)
    stopifnot(longest_common_subsequence("programming", "gaming") == 6)
    stopifnot(longest_common_subsequence("", "abc") == 0)

}

# Run tests
main()