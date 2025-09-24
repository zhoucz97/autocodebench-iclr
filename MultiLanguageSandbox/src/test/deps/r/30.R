


# This function, minimum_score_merge, calculates the minimum score required to merge N piles of stones into a single pile. The score for each merge is the sum of the stones in the two piles being merged. The function takes a vector representing the number of stones in each pile and returns the minimum total score needed for all merges.
# Examples:
# >>> minimum_score_merge(c(13, 7, 8, 16, 21, 4, 18))
#     239

minimum_score_merge <- function(stone_piles) {



    total_score <- 0
    piles <- as.list(stone_piles)
    while (length(piles) > 1) {
        piles <- sort(unlist(piles), decreasing = FALSE)
        piles <- as.list(piles)
        first <- piles[[1]]
        second <- piles[[2]]
        piles <- piles[-c(1, 2)]
        merge_score <- first + second
        total_score <- total_score + merge_score
        piles <- c(piles, list(merge_score))
    }
    
    return(total_score)
}
# Test cases
main <- function() {

    stopifnot(minimum_score_merge(c(13, 7, 8, 16, 21, 4, 18)) == 239)
    stopifnot(minimum_score_merge(c(10, 20, 30)) == 90)
    stopifnot(minimum_score_merge(c(1, 2, 3, 4)) == 19)
}


# Run tests
main()