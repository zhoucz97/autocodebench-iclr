

# This function book_purchase_options calculates the number of different ways Xiao Ming can purchase books with a given amount of money. 
# The available book prices are 10, 20, 50, and 100 yuan. Xiao Ming can purchase any number of books of any type as long as he does not exceed the total money he has.
# Examples:
# >>> book_purchase_options(20)
#     2
# >>> book_purchase_options(30)
#     3

book_purchase_options <- function(money) {
    book_prices <- c(10, 20, 50, 100)
    dp <- integer(money + 1)
    dp[1] <- 1
    
    for (price in book_prices) {
        for (i in (price):(money)) {
            dp[i + 1] <- dp[i + 1] + dp[i + 1 - price]
        }
    }
    
    return(dp[money + 1])
}
# Test cases
main <- function() {
stopifnot(book_purchase_options(20) == 2)
stopifnot(book_purchase_options(30) == 2)
stopifnot(book_purchase_options(0) == 1)
stopifnot(book_purchase_options(100) == 11)



}


# Run tests
main()