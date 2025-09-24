


# This function max_ascending_subseq_sum calculates the maximum sum of an ascending subsequence within a given sequence of integers. An ascending subsequence is defined as a sequence where each subsequent number is greater than the previous one. The function takes a numeric vector as input and returns the maximum sum of any ascending subsequence within this vector.
# Examples:
# >>> max_ascending_subseq_sum(c(1, 7, 3, 5, 9, 4, 8))
#     18
# >>> max_ascending_subseq_sum(c(100, 1, 2, 3))
#     100

max_ascending_subseq_sum <- function(sequence) {
  if (length(sequence) == 0) return(0)
  
  max_sum <- sequence[1]
  current_sum <- sequence[1]
  
  for (i in 2:length(sequence)) {
    if (sequence[i] > sequence[i-1]) {
      current_sum <- current_sum + sequence[i]
    } else {
      current_sum <- sequence[i]
    }
    
    if (current_sum > max_sum) {
      max_sum <- current_sum
    }
  }
  
  return(max_sum)
}
# Test cases
main <- function() {

    stopifnot(max_ascending_subseq_sum(c(1, 7, 3, 5, 9, 4, 8)) == 18)
    stopifnot(max_ascending_subseq_sum(c(100, 1, 2, 3)) == 100)
    stopifnot(max_ascending_subseq_sum(c(10, 20, 30, 5, 6)) == 60)


}

# Run tests
main()