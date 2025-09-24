


# This function max_scenic_views determines the maximum number of scenic spots that can be visited under certain conditions. Given n as the number of scenic spots and a vector altitudes representing the altitude of each spot, the function returns the maximum number of spots that can be visited. The rules are:

# The spots must be visited in increasing order of their IDs (i.e., visit a spot with a higher ID than the previous).
# Consecutive spots with the same altitude cannot be visited.
# Once the descent starts (visiting a spot at a lower altitude), it's not allowed to go back up.
# Examples:

# max_scenic_views(8, c(186, 186, 150, 200, 160, 130, 197, 220)) should return 4.


max_scenic_views <- function(n, altitudes) {
    if (n == 0) return(0)
    
    max_spots <- 1
    current_spots <- 1
    i <- 2
    
    while (i <= n) {
        if (altitudes[i] != altitudes[i - 1]) {
            if (altitudes[i] > altitudes[i - 1]) {
                current_spots <- current_spots + 1
            } else {

                current_spots <- 2
            }
            max_spots <- max(max_spots, current_spots)
        }
        i <- i + 1
    }
    
    return(max_spots)
}
# Test cases
main <- function() {

  stopifnot(max_scenic_views(8, c(186, 186, 150, 200, 160, 130, 197, 220)) == 4)
  stopifnot(max_scenic_views(5, c(100, 120, 110, 130, 140)) == 4)
  stopifnot(max_scenic_views(4, c(100, 100, 100, 100)) == 1)


}


# Run tests
main()