

# This function decimal_to_hexadecimal converts a given non-negative integer from its decimal form to its hexadecimal representation. Hexadecimal numbers use 16 symbols: 0-9 and A-F, where A-F represent decimal numbers 10-15 respectively. In hexadecimal, the counting method is base 16.
# Examples:
# decimal_to_hexadecimal(30): returns '1E'.
# decimal_to_hexadecimal(17): returns '11'.


decimal_to_hexadecimal <- function(decimal_number) {
  if (decimal_number < 0) {
    stop("Input must be a non-negative integer.")
  }
  
  if (decimal_number == 0) {
    return("0")
  }
  
  hex_digits <- c(0:9, LETTERS[1:6])
  hexadecimal <- ""
  
  while (decimal_number > 0) {
    remainder <- decimal_number %% 16
    hexadecimal <- paste0(hex_digits[remainder + 1], hexadecimal)
    decimal_number <- floor(decimal_number / 16)
  }
  
  return(hexadecimal)
}
# Test cases
main <- function() {
    stopifnot(decimal_to_hexadecimal(30) == '1E')
    stopifnot(decimal_to_hexadecimal(17) == '11')
    stopifnot(decimal_to_hexadecimal(256) == '100')
    stopifnot(decimal_to_hexadecimal(0) == '0')
    stopifnot(decimal_to_hexadecimal(2147483647) == '7FFFFFFF')
    stopifnot(decimal_to_hexadecimal(45) == '2D')
}

# Run tests
main()