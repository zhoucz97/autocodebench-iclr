

# This function first_ten_digits_of_power_of_two calculates the first ten digits of 2 raised to the power of n. Given a natural number n, the function returns the first ten digits of 2^n.
# It handles cases where 0 <= n <= 10000000.
# Examples:
# first_ten_digits_of_power_of_two(60): This returns 1152921504, which are the first ten digits of 2^60.
# first_ten_digits_of_power_of_two(10): Returns 1024, as 2^10 is 1024, which is less than ten digits and hence, the function returns the number itself.

first_ten_digits_of_power_of_two <- function(n) {
  if (n == 0) {
    return(1)
  }
  power <- pow.bigz(2, n)
  power_str <- as.character(power)
  len <- nchar(power_str)
  if (len <= 10) {
    return(as.numeric(power_str))
  } else {
    first_ten <- substr(power_str, 1, 10)
    return(as.numeric(first_ten))
  }
}
# Test cases
main <- function() {
    stopifnot(first_ten_digits_of_power_of_two(60) == 1152921504)
    stopifnot(first_ten_digits_of_power_of_two(10) == 1024)
    stopifnot(first_ten_digits_of_power_of_two(0) == 1)
    stopifnot(first_ten_digits_of_power_of_two(3) == 8)
    stopifnot(first_ten_digits_of_power_of_two(20) == 1048576)
    stopifnot(first_ten_digits_of_power_of_two(50) == 1125899906)
}

# Run tests
main()