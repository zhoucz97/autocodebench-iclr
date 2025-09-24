

# This function count_composition_title_characters calculates the number of characters in the title of a composition, excluding spaces and newline characters. The input is a string representing the title, and the output is an integer representing the count of characters that are not spaces or newline characters.
# Example 1:
# Input: "234"
# Output: 3

# Example 2:
# Input: "Ca 45"
# Output: 4

count_composition_title_characters <- function(title){
  cleaned_title <- gsub("[ \n]", "", title)
  return(nchar(cleaned_title))
}
# Test cases
main <- function() {
    stopifnot(count_composition_title_characters("234") == 3)
    stopifnot(count_composition_title_characters("Ca 45") == 4)
    stopifnot(count_composition_title_characters("Hello World\n") == 10)
    stopifnot(count_composition_title_characters(" 123 ") == 3)

}


# Run tests
main()