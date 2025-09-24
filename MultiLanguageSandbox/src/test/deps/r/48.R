


# This function calculates the total earnings of Deliv-e-droid based on the number of successfully delivered and failed deliveries. For each successful delivery, Deliv-e-droid earns 50 units of money, and for each failed delivery, it loses 10 units. If the number of successful deliveries is greater than the number of failed ones, Deliv-e-droid receives an additional bonus of 500 units.
# Arguments:
# - delivered: The number of successfully delivered packages (integer, 0 <= delivered <= 100).
# - failed: The number of failed deliveries (integer, 0 <= failed <= 100).
# Returns:
# - The total earnings of Deliv-e-droid (integer), which can be negative if losses from failed deliveries outweigh gains from successful ones.
# Examples:
# >>> calculate_droid_earnings(5, 2)
#     730
# >>> calculate_droid_earnings(0, 10)
#     -100

calculate_droid_earnings <- function(delivered, failed) {
  earnings <- delivered * 50
  earnings <- earnings - (failed * 10)
  if (delivered > failed) {
    earnings <- earnings + 500
  }
  
  return(earnings)
}
# Test cases
main <- function() {

    stopifnot(calculate_droid_earnings(5, 2) == 730)
    stopifnot(calculate_droid_earnings(0, 10) == -100)
    stopifnot(calculate_droid_earnings(10, 0) == 1000)
    stopifnot(calculate_droid_earnings(3, 3) == 120)

}


# Run tests
main()