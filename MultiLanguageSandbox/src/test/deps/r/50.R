

# This function count_chars_types analyzes a given string of length 8, counting the occurrences of digits, lowercase letters, and uppercase letters within it. The function returns a list with three elements corresponding to the counts of digits, lowercase letters, and uppercase letters, respectively.

# # Example 1
# count_chars_types("yLOI2022")
# # Returns: list(digits = 4, lowercase = 1, uppercase = 3)

# # Example 2
# count_chars_types("IAKIOIOI")
# # Returns: list(digits = 0, lowercase = 0, uppercase = 8)

# # Example 3
# count_chars_types("1n2s0e1s")
# # Returns: list(digits = 4, lowercase = 4, uppercase = 0)


count_chars_types <- function(s) {
  if (nchar(s) != 8) {
    stop("The input string must be of length 8.")
  }
  digits <- 0
  lowercase <- 0
  uppercase <- 0
  for (char in strsplit(s, "")[[1]]) {
    if (grepl("[0-9]", char)) {
      digits <- digits + 1
    } else if (grepl("[a-z]", char)) {
      lowercase <- lowercase + 1
    } else if (grepl("[A-Z]", char)) {
      uppercase <- uppercase + 1
    }
  }
  return(list(digits = digits, lowercase = lowercase, uppercase = uppercase))
}
# Test cases
main <- function() {
    stopifnot(identical(count_chars_types("yLOI2022"), list(digits = 4, lowercase = 1, uppercase = 3)))
    stopifnot(identical(count_chars_types("IAKIOIOI"), list(digits = 0, lowercase = 0, uppercase = 8)))
    stopifnot(identical(count_chars_types("1n2s0e1s"), list(digits = 4, lowercase = 4, uppercase = 0)))

}


# Run tests
main()