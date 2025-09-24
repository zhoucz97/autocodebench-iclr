

# This function detect_anagram_keywords is designed to identify keywords in a sentence even if their letters have been rearranged. The function takes two arguments: a vector of keywords and a sentence. The sentence and keywords are both strings. The function returns a vector containing the rearranged keywords found in the sentence.
# The keywords and sentence are assumed to be lowercase and separated by spaces. The function does not consider word boundaries when searching for anagrams.
# Examples:
# detect_anagram_keywords(c("apple", "banana", "cherry"), "elppa is delicious and so is nanaab"): This should return c("apple", "banana").

detect_anagram_keywords <- function(keywords, sentence) {
  chars_in_sentence <- unlist(strsplit(gsub(" ", "", sentence), ""))
  get_char_counts <- function(s) {
    chars <- unlist(strsplit(s, ""))
    table(chars)
  }
  keyword_counts <- lapply(keywords, get_char_counts)
  found_keywords <- c()
  for (i in seq_along(keywords)) {
    keyword <- keywords[i]
    counts <- keyword_counts[[i]]
    all_chars_present <- TRUE
    for (char in names(counts)) {
      if (is.null(counts[char]) || 
          sum(chars_in_sentence == char) < counts[char]) {
        all_chars_present <- FALSE
        break
      }
    }
    
    if (all_chars_present) {
      found_keywords <- c(found_keywords, keyword)
    }
  }
  
  return(found_keywords)
}
# Test cases
main <- function() {

    stopifnot(identical(detect_anagram_keywords(c("apple", "banana", "cherry"), "elppa is delicious and so is nanaab"), c("apple", "banana")))
    stopifnot(identical(detect_anagram_keywords(c("cat", "dog", "bird"), "tac and god are friends"), c("cat", "dog")))
    stopifnot(identical(detect_anagram_keywords(c("moon", "star", "sun"), "noom lights the night sky"), c("moon")))
    stopifnot(identical(detect_anagram_keywords(c("blue", "green", "red"), "eelb sky and nedreg grass"), c()))
    stopifnot(identical(detect_anagram_keywords(c("fish", "shark", "whale"), "hsif in the sea"), c("fish")))

}

# Run tests
main()