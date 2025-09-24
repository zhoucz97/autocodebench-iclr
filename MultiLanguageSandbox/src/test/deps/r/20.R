


# This function, most_frequent_value, identifies the most frequently occurring value in a sorted vector of integers. If there are multiple values with the same highest frequency, it returns the smallest of these values. The input is a sorted vector of integers, and the function returns the integer that appears most frequently.

# Examples:

# most_frequent_value(c(3, 3, 4, 4, 4, 5)): Returns 4.
# most_frequent_value(c(10, 20, 20, 20, 30)): Returns 20.
# most_frequent_value(c(1, 1, 2, 2, 3)): Returns 1.

most_frequent_value <- function(sorted_values) {
  if (length(sorted_values) == 0) {
    stop("Input vector is empty")
  }
  max_freq <- 1
  current_freq <- 1
  most_frequent <- sorted_values[1]
  current_value <- sorted_values[1]
  for (i in 2:length(sorted_values)) {
    if (sorted_values[i] == current_value) {
      current_freq <- current_freq + 1
    } else {
      if (current_freq > max_freq || (current_freq == max_freq && current_value < most_frequent)) {
        max_freq <- current_freq
        most_frequent <- current_value
      }
      current_value <- sorted_values[i]
      current_freq <- 1
    }
  }
  if (current_freq > max_freq || (current_freq == max_freq && current_value < most_frequent)) {
    most_frequent <- current_value
  }
  
  return(most_frequent)
}
# Test cases
main <- function() {

    stopifnot(most_frequent_value(c(3, 3, 4, 4, 4, 5)) == 4)
    stopifnot(most_frequent_value(c(10, 20, 20, 20, 30)) == 20)
    stopifnot(most_frequent_value(c(1, 1, 2, 2, 3)) == 1)
    stopifnot(most_frequent_value(c(100, 150, 150, 200, 250)) == 150)
    stopifnot(most_frequent_value(c(7, 8, 8, 9)) == 8)
    stopifnot(most_frequent_value(c()) == NULL)

}

# Run tests
main()