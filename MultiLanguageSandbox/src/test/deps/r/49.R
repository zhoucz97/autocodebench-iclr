

# This function find_missing_and_duplicate_id identifies the missing and duplicate ID numbers from a list of ID sequences. Given an integer N representing the number of ID sequences and a list of ID sequences, the function returns a vector containing two integers: the missing ID and the duplicate ID.
# The input is an integer N and a list where each element is a vector representing an ID sequence.
# The output is a vector with two integers: the first is the missing ID, and the second is the duplicate ID.
# Examples:
# >>> find_missing_and_duplicate_id(2, list(c(5, 6, 8, 11, 9), c(10, 12, 9)))
#     c(7, 9)

find_missing_and_duplicate_id <- function(N, id_lists) {
    all_ids <- unlist(id_lists)
    duplicate_id <- NULL
    for (id in all_ids) {
        if (sum(all_ids == id) > 1) {
            duplicate_id <- id
            break
        }
    }
    expected_ids <- 1:N
    missing_id <- setdiff(expected_ids, all_ids)
    return(c(missing_id, duplicate_id))
}
# Test cases
main <- function() {

stopifnot(all(find_missing_and_duplicate_id(2, list(c(5, 6, 8, 11, 9), c(10, 12, 9))) == c(7, 9)))
stopifnot(all(find_missing_and_duplicate_id(3, list(c(1, 2, 4, 9), c(5, 6, 7, 8, 10), c(9, 11))) == c(3, 9)))


}


# Run tests
main()