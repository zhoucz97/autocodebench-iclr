


# This function `next_binary_number_with_same_ones` finds the smallest positive integer greater than N, which has the same number of 1s in its binary representation as N. 
# It takes an integer N as input and returns the next integer that meets the criteria.
# Examples:
# >>> next_binary_number_with_same_ones(1)
#     2
# >>> next_binary_number_with_same_ones(78)
#     83


next_binary_number_with_same_ones <- function(N) {
    if (N <= 0) {
        stop("N must be a positive integer")
    }
    count_ones <- function(x) {
        sum(as.integer(intToBits(x)))
    }
    
    target_ones <- count_ones(N)
    current <- N + 1
    
    while (TRUE) {
        if (count_ones(current) == target_ones) {
            return(current)
        }
        current <- current + 1
    }
}
# Test cases
main <- function() {

    stopifnot(next_binary_number_with_same_ones(1) == 2)
    stopifnot(next_binary_number_with_same_ones(2) == 4)
    stopifnot(next_binary_number_with_same_ones(3) == 5)
    stopifnot(next_binary_number_with_same_ones(4) == 8)
    stopifnot(next_binary_number_with_same_ones(78) == 83)

}


# Run tests
main()