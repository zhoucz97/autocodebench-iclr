

# This function, decimal_to_binary, takes a vector of decimal integers and converts each to its corresponding binary representation. The function returns a list where each element is a string showing the decimal number followed by "-->", and then its binary representation.
# Negative numbers are handled by converting their absolute value to binary and then prefixing the result with a '-'.
# Examples:
# >>> decimal_to_binary(c(2, 0, -12, 1))
#     [1] "2-->10" "0-->0" "-12-->-1100" "1-->1"

decimal_to_binary <- function(decimal_numbers) {
  decimal_to_binary_single <- function(n) {
    if (n == 0) {
      return("0")
    }
    is_negative <- n < 0
    n <- abs(n)
    binary <- ""
    while (n > 0) {
      binary <- paste0(n %% 2, binary)
      n <- n %/% 2
    }
    if (is_negative) {
      binary <- paste0("-", binary)
    }
    return(binary)
  }
  result <- sapply(decimal_numbers, function(x) {
    paste0(x, "-->", decimal_to_binary_single(x))
  })
  
  return(result)
}
# Test cases
main <- function() {

    stopifnot(all.equal(decimal_to_binary(c(2, 0, -12, 1)), c("2-->10", "0-->0", "-12-->-1100", "1-->1")))
    stopifnot(decimal_to_binary(c(3)) == "3-->11")
    stopifnot(decimal_to_binary(c(-5)) == "-5-->-101")
}


# Run tests
main()