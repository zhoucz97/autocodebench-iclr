

# The function maximum_theft_profit calculates the maximum amount of cash that can be stolen without triggering the alarm system. Given a vector representing the amount of cash in each shop on a street, it returns the maximum cash that can be stolen without robbing two adjacent shops, as robbing adjacent shops would trigger the alarm.
# Examples:
# >>> maximum_theft_profit(c(1, 8, 2))
#     8
# >>> maximum_theft_profit(c(10, 7, 6, 14))
#     24

maximum_theft_profit <- function(shop_cash) {
  n <- length(shop_cash)
  if (n == 0) {
    return(0)
  }
  if (n == 1) {
    return(shop_cash[1])
  }
  dp <- numeric(n)
  dp[1] <- shop_cash[1]
  dp[2] <- max(shop_cash[1], shop_cash[2])
  for (i in 3:n) {


    dp[i] <- max(dp[i - 1], dp[i - 2] + shop_cash[i])
  }
  return(dp[n])
}
# Test cases
main <- function() {

    stopifnot(maximum_theft_profit(c(1, 8, 2)) == 8)
    stopifnot(maximum_theft_profit(c(10, 7, 6, 14)) == 24)
    stopifnot(maximum_theft_profit(c(0, 0, 0, 0)) == 0)
    stopifnot(maximum_theft_profit(c(5)) == 5)
}


# Run tests
main()