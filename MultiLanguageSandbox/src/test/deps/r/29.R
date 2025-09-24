


# This function, calculate_string_distance, computes the "distance" between two strings. The distance is defined as the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into the other. 
# This kind of measurement is often known as Levenshtein distance.
# Examples:
# >>> calculate_string_distance("abcdefg", "abcdef")
#     1
# >>> calculate_string_distance("ab", "ab")
#     0
# >>> calculate_string_distance("mnklj", "jlknm")
#     4

calculate_string_distance <- function(string1, string2) {
  s1 <- unlist(strsplit(string1, ""))
  s2 <- unlist(strsplit(string2, ""))
  
  len1 <- length(s1)
  len2 <- length(s2)
  dist_matrix <- matrix(0, nrow = len1 + 1, ncol = len2 + 1)
  for (i in 1:(len1 + 1)) {
    dist_matrix[i, 1] <- i - 1
  }
  for (j in 1:(len2 + 1)) {
    dist_matrix[1, j] <- j - 1
  }
  for (i in 2:(len1 + 1)) {
    for (j in 2:(len2 + 1)) {
      cost <- ifelse(s1[i - 1] == s2[j - 1], 0, 1)
      dist_matrix[i, j] <- min(
        dist_matrix[i - 1, j] + 1,
        dist_matrix[i, j - 1] + 1,
        dist_matrix[i - 1, j - 1] + cost
      )
    }
  }
  return(dist_matrix[len1 + 1, len2 + 1])
}
# Test cases
main <- function() {

    stopifnot(calculate_string_distance("abcdefg", "abcdef") == 1)
    stopifnot(calculate_string_distance("abcfbc", "abfcab") == 3)
    stopifnot(calculate_string_distance("programming", "contest") == 10)
    stopifnot(calculate_string_distance("ab", "ab") == 0)
    stopifnot(calculate_string_distance("mnklj", "jlknm") == 4)


}


# Run tests
main()