

# This function dance_partner_matching simulates the pairing of dance partners at a ballroom dance. 
# Given the number of men and women entering the ballroom and the number of dances to be held, 
# it returns the pairing sequence for each dance. Each dance pairs one man with one woman. 
# If there are more participants in one group, those without a partner wait for the next dance.
# The function returns a matrix, where each row represents a dance and the columns represent the paired man and woman.
# Examples:
# >>> dance_partner_matching(4, 6, 7)
#       [,1] [,2]
# [1,]    1    1
# [2,]    2    2
# [3,]    3    3
# [4,]    4    4
# [5,]    1    5
# [6,]    2    6
# [7,]    3    1

dance_partner_matching <- function(men_count, women_count, dance_count) {
  men_sequence <- rep(1:men_count, length.out = dance_count)
  women_sequence <- rep(1:women_count, length.out = dance_count)
  pairing_matrix <- matrix(nrow = dance_count, ncol = 2)
  colnames(pairing_matrix) <- c("Man", "Woman")
  for (i in 1:dance_count) {
    pairing_matrix[i, ] <- c(men_sequence[i], women_sequence[i])
  }
  
  return(pairing_matrix)
}
# Test cases
main <- function() {
    # Testcase
    test_pairing <- dance_partner_matching(4, 6, 7)
    expected_pairing <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 1, 5, 2, 6, 3, 1), ncol = 2, byrow = TRUE)
    stopifnot(identical(test_pairing, expected_pairing))

}


# Run tests
main()