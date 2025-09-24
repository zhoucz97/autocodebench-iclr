

# This function hex_to_octal takes a vector of hexadecimal numbers (as strings) and converts each to its corresponding octal representation. The function returns a vector of octal numbers as strings. Each hexadecimal number is composed of characters ranging from 0 to 9 and A to F, representing values from 10 to 15 respectively. The input vector can contain up to 10 hexadecimal numbers, each up to 100000 characters long.

# Examples:

# hex_to_octal(c("1A", "2B")): This would return c("32", "53") as the octal representations of 1A and 2B in hexadecimal.
# hex_to_octal(c("FF")): This would return c("377") as the octal representation of FF in hexadecimal.

hex_to_octal <- function(hex_numbers) {
  octal_numbers <- character(length(hex_numbers))
  
  for (i in seq_along(hex_numbers)) {
    hex <- hex_numbers[i]
    decimal <- strtoi(hex, base = 16)
    if (decimal == 0) {
      octal_numbers[i] <- "0"
      next
    }
    octal <- ""
    while (decimal > 0) {
      remainder <- decimal %% 8
      octal <- paste0(remainder, octal)
      decimal <- floor(decimal / 8)
    }
    
    octal_numbers[i] <- octal
  }
  
  return(octal_numbers)
}
# Test cases
main <- function() {
    stopifnot(identical(hex_to_octal(c("1A", "2B")), c("32", "53")))
    stopifnot(identical(hex_to_octal(c("FF")), c("377")))
    stopifnot(identical(hex_to_octal(c("ABC")), c("5274")))
    stopifnot(identical(hex_to_octal(c("0")), c("0")))
    stopifnot(identical(hex_to_octal(c("10", "20", "30")), c("20", "40", "60")))


}

# Run tests
main()