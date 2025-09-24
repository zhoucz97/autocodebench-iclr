

#   Check if in given list of numbers, are any two numbers closer to each other than
#   given threshold.
#   >>> has_close_elements([1.0, 2.0, 3.0], 0.5)
#   False
#   >>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)
#   True
has_close_elements <- function(numbers, threshold) {
  sorted_numbers <- sort(numbers)
  for (i in 1:(length(sorted_numbers) - 1)) {
    if (sorted_numbers[i + 1] - sorted_numbers[i] < threshold) {
      return(TRUE)
    }
  }
  return(FALSE)
}
# Test cases
test_has_close_elements <- function() {
  stopifnot(has_close_elements(c(1.0, 2.0, 3.9, 4.0, 5.0, 2.2), 0.3) == TRUE)
  stopifnot(has_close_elements(c(1.0, 2.0, 3.9, 4.0, 5.0, 2.2), 0.05) == FALSE)
  stopifnot(has_close_elements(c(1.0, 2.0, 5.9, 4.0, 5.0), 0.95) == TRUE)
  stopifnot(has_close_elements(c(1.0, 2.0, 5.9, 4.0, 5.0), 0.8) == FALSE)
  stopifnot(has_close_elements(c(1.0, 2.0, 3.0, 4.0, 5.0, 2.0), 0.1) == TRUE)
  stopifnot(has_close_elements(c(1.1, 2.2, 3.1, 4.1, 5.1), 1.0) == TRUE)
  stopifnot(has_close_elements(c(1.1, 2.2, 3.1, 4.1, 5.1), 0.5) == FALSE)
  
  cat("All tests passed!\n")
}

# Run tests
test_has_close_elements()