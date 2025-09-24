


# This function second_smallest_number computes the second smallest unique number in a given vector of integers, considering each integer separately, even if there are duplicates. The function iterates through the vector, identifying the smallest and the second smallest numbers. Sorting of integers is not allowed !
# For example:
# >> second_smallest_number(c(1, 1, 3))
#    1
   
second_smallest_number <- function(numbers) {
  unique_numbers <- unique(numbers)
  if (length(unique_numbers) < 2) {
    return(NA)
  }
  smallest <- Inf
  second_smallest <- Inf
  for (num in unique_numbers) {
    if (num < smallest) {
      second_smallest <- smallest
      smallest <- num
    } else if (num < second_smallest && num != smallest) {
      second_smallest <- num
    }
  }
  
  return(second_smallest)
}
# Test cases
main <- function() {
    stopifnot(second_smallest_number(c(1, 1, 3)) == 1)
    stopifnot(second_smallest_number(c(2, 3, 4)) == 3)
    stopifnot(second_smallest_number(c(5, 7, 8, 9)) == 7)
    stopifnot(second_smallest_number(c(3, 3, 3, 3)) == 3)
    stopifnot(second_smallest_number(c(15, 10)) == 15)

}

# Run tests
main()