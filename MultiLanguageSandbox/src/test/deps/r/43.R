


# This function `longest_consecutive_sequence` calculates the length of the longest consecutive numbers sequence within a given vector of positive integers. A consecutive sequence is defined as a series of numbers in which each number is one more than the previous number.
# Examples:
# >>> longest_consecutive_sequence(c(1, 5, 6, 2, 3, 4, 5, 6, 8, 9))
#     6  # Because the longest consecutive sequence is 1, 2, 3, 4, 5, 6
# >>> longest_consecutive_sequence(c(10, 11, 12, 14, 15))
#     3  # Because the longest consecutive sequence is 10, 11, 12

longest_consecutive_sequence <- function(numbers) {
  unique_numbers <- sort(unique(numbers))
  
  if (length(unique_numbers) == 0) {
    return(0)
  }
  
  max_length <- 1
  current_length <- 1
  
  for (i in 2:length(unique_numbers)) {
    if (unique_numbers[i] == unique_numbers[i - 1] + 1) {
      current_length <- current_length + 1
      if (current_length > max_length) {
        max_length <- current_length
      }
    } else {
      current_length <- 1
    }
  }
  
  return(max_length)
}
# Test cases
main <- function() {

    stopifnot(longest_consecutive_sequence(c(1, 5, 6, 2, 3, 4, 5, 6, 8, 9)) == 6)
    stopifnot(longest_consecutive_sequence(c(10, 11, 12, 14, 15)) == 3)
    stopifnot(longest_consecutive_sequence(c(4, 2, 1, 6, 5)) == 3)
    stopifnot(longest_consecutive_sequence(c()) == 0)  # Empty vector
    stopifnot(longest_consecutive_sequence(c(7)) == 1)  # Single element


}


# Run tests
main()