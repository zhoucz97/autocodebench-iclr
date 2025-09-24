

# This function calculates the maximum number of cigarettes Peter can smoke.
# Given the initial number of cigarettes 'n' and the number 'k' of cigarette butts required to exchange for one new cigarette,
# the function returns the total number of cigarettes Peter can smoke.
# It includes the initial cigarettes and those obtained by exchanging butts.
# Examples:
# >>> maximum_smokable_cigarettes(4, 3)
#     5
# >>> maximum_smokable_cigarettes(10, 3)
#     14

maximum_smokable_cigarettes <- function(n, k) {
  total <- n
  butts <- n
  
  while (butts >= k) {
    new_cigs <- floor(butts / k)
    total <- total + new_cigs
    butts <- butts %% k + new_cigs
  }
  
  return(total)
}
# Test cases
main <- function() {
    stopifnot(maximum_smokable_cigarettes(4, 3) == 5)
    stopifnot(maximum_smokable_cigarettes(10, 3) == 14)
    stopifnot(maximum_smokable_cigarettes(1, 2) == 1) # Case where no exchanges can occur
}


# Run tests
main()