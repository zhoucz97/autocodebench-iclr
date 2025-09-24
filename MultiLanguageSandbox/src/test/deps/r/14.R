

# This function calculate_word_score computes the total score of a given word based on the position of its letters in the alphabet. Each letter's score is its position in the alphabet (A/a = 1, B/b = 2, ..., Z/z = 26), and the function sums these values to get the total score of the word.
# The word should contain only uppercase and lowercase letters.
# Examples:
# calculate_word_score("Hello"): This should return 52, as H(8) + e(5) + l(12) + l(12) + o(15) = 52.


calculate_word_score <- function(word) {
  word_upper <- toupper(word)
  chars <- strsplit(word_upper, "")[[1]]
  scores <- sapply(chars, function(char) {
    ascii_value <- utf8ToInt(char)
    position <- ascii_value - 64
    return(position)
  })
  total_score <- sum(scores)
  
  return(total_score)
}
# Test cases
main <- function() {
    stopifnot(calculate_word_score("Hello") == 52)
    stopifnot(calculate_word_score("abcABC") == 12)
    stopifnot(calculate_word_score("Zz") == 52)
    stopifnot(calculate_word_score("R") == 18)
    stopifnot(calculate_word_score("AttiTude") == 100)
}

# Run tests
main()