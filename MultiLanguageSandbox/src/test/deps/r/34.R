

# This function calculates the number of different ways Naruto can distribute his chakra energy (M) across a maximum number of shadow clones (N).
# The number of ways is denoted by K. A shadow clone can be allocated zero or more units of chakra energy.
# The function takes two arguments: chakra_energy (M) and max_clones (N), and returns the number of distribution methods (K).
# Examples:
# >>> shadow_clone_variations(7, 3)
#     8
# >>> shadow_clone_variations(5, 2)
#     6

shadow_clone_variations <- function(chakra_energy, max_clones) {
    M <- chakra_energy
    N <- max_clones
    if (N == 0) {
        return(0)
    }
    k <- M + N - 1
    n <- N - 1

    if (n > k - n) {
        n <- k - n
    }
    result <- 1
    for (i in 1:n) {
        result <- result * (k - n + i) / i
    }
    return(result)
}
# Test cases
main <- function() {

    stopifnot(shadow_clone_variations(7, 3) == 8)
    stopifnot(shadow_clone_variations(5, 2) == 3)
    stopifnot(shadow_clone_variations(3, 3) == 3)

}


# Run tests
main()