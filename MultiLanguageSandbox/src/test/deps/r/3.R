

# This function minimum_voter_support calculates the minimum number of voters needed to win a majority of states in an election. Given a vector representing the number of voters in each state, the function returns the smallest number of voters required to secure a majority of the states, assuming that winning over half the voters in a state secures it.

# Examples:
# minimum_voter_support(c(10, 20, 15)): In this case, the function returns 14.

minimum_voter_support <- function(states_voters) {
    num_states <- length(states_voters)
    states_needed <- ceiling(num_states / 2)
    voters_needed_per_state <- ceiling(states_voters / 2)
    sorted_voters_needed <- sort(voters_needed_per_state)
    total_voters <- sum(sorted_voters_needed[1:states_needed])
    
    return(total_voters)
}
# Test cases
main <- function() {
    stopifnot(minimum_voter_support(c(10, 20, 15)) == 14)
    stopifnot(minimum_voter_support(c(5, 7, 5)) == 6)
    stopifnot(minimum_voter_support(c(6, 6, 6, 6)) == 12)
    stopifnot(minimum_voter_support(c(10, 10, 10, 10, 10, 11)) == 24)
    stopifnot(minimum_voter_support(c(1, 1, 1, 1, 100)) == 3)
    stopifnot(minimum_voter_support(c(50, 50, 50, 51)) == 78)

}

# Run tests
main()