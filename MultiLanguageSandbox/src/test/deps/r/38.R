

# This function remove_spaces takes a vector of strings as input and returns a new vector where all spaces have been removed from each string.
# The function operates on each string individually, removing any spaces it contains. gsub() is not allowed ! 
# Examples:
# >>> remove_spaces(c("Hello World", "1 2 3", "Nice to meet you", "abc"))
#     c("HelloWorld", "123", "Nicetomeetyou", "abc")

remove_spaces <- function(input_strings){
  chars_list <- strsplit(input_strings, "")
  no_space_chars_list <- lapply(chars_list, function(chars) {
    chars[chars != " "]
  })
  result <- sapply(no_space_chars_list, paste0, collapse = "")
  
  return(result)
}
# Test cases
main <- function() {
    # Testcase
    stopifnot(identical(remove_spaces(c("Hello World", "1 2 3", "Nice to meet you", "abc")), 
                        c("HelloWorld", "123", "Nicetomeetyou", "abc")))
    stopifnot(identical(remove_spaces(c("Test Case", "With  Spaces", "NoSpaces")), 
                        c("TestCase", "WithSpaces", "NoSpaces")))
    stopifnot(identical(remove_spaces(c("  Leading", "Trailing ", " Both ")), 
                        c("Leading", "Trailing", "Both")))


}


# Run tests
main()