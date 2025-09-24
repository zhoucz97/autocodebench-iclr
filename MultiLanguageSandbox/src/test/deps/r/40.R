

# The function 'find_mode_and_frequency' calculates the mode and its frequency in a given vector of natural numbers.
# It takes a vector 'numbers' as input and returns a list with two elements: the mode and its frequency.
# The mode is defined as the number that appears most frequently in the vector. 
# If there are multiple modes, the function returns the smallest one.
# Examples:
# >>> find_mode_and_frequency(c(1, 2, 2, 2, 3, 5))
#     $mode
#     [1] 2
#     $frequency
#     [1] 3
# >>> find_mode_and_frequency(c(4, 4, 5, 5, 6))
#     $mode
#     [1] 4
#     $frequency
#     [1] 2


find_mode_and_frequency <- function(numbers) {
  freq_table <- table(numbers)
  max_freq <- max(freq_table)
  modes <- as.numeric(names(freq_table[freq_table == max_freq]))
  mode_value <- min(modes)
  return(list(mode = mode_value, frequency = max_freq))
}
# Test cases
main <- function() {

    # Test case 1
    test_data_1 <- c(1, 2, 2, 2, 3, 5)
    result_1 <- find_mode_and_frequency(test_data_1)
    stopifnot(result_1$mode == 2)
    stopifnot(result_1$frequency == 3)

    # Test case 2
    test_data_2 <- c(4, 4, 5, 5, 6)
    result_2 <- find_mode_and_frequency(test_data_2)
    stopifnot(result_2$mode == 4)
    stopifnot(result_2$frequency == 2)

    # Test case 3
    test_data_3 <- c(7, 8, 8, 9, 9, 9)
    result_3 <- find_mode_and_frequency(test_data_3)
    stopifnot(result_3$mode == 9)
    stopifnot(result_3$frequency == 3)

}


# Run tests
main()