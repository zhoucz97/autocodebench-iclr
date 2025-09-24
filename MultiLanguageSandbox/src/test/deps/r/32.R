

# This function, maximum_profit, calculates the maximum profit that can be obtained by buying and selling a stock at most twice. The input is a vector representing the stock prices over a number of days.
# The function returns the maximum profit that can be achieved under the constraint that the second buy can only happen after the first sell.
# The stock can be bought and sold on the same day.
# Examples:
# >>> maximum_profit(c(5, 14, -2, 4, 9, 3, 17))
#     28
# >>> maximum_profit(c(6, 8, 7, 4, 1, -2))
#     2
# >>> maximum_profit(c(18, 9, 5, 2))
#     0

maximum_profit <- function(price_sequence) {
    if (length(price_sequence) < 2) {
        return(0)
    }
    
    buy1 <- -Inf
    sell1 <- 0
    buy2 <- -Inf
    sell2 <- 0
    
    for (price in price_sequence) {
        buy1 <- max(buy1, -price)
        sell1 <- max(sell1, buy1 + price)
        buy2 <- max(buy2, sell1 - price)
        sell2 <- max(sell2, buy2 + price)
    }
    
    return(max(0, sell2))
}
# Test cases
main <- function() {

    stopifnot(maximum_profit(c(5, 14, -2, 4, 9, 3, 17)) == 28)
    stopifnot(maximum_profit(c(6, 8, 7, 4, 1, -2)) == 2)
    stopifnot(maximum_profit(c(18, 9, 5, 2)) == 0)


}


# Run tests
main()