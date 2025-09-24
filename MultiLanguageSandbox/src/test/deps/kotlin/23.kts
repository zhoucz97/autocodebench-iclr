
/**
 * Calculates the number of ways to exchange a given amount of money (more than 10 units) into 5, 2, and 1 unit denominations.
 * It is required that all three denominations are used in the exchange.
 * Example:
 * >>> countExchangeWays(50)
 *     106
 * >>> countExchangeWays(20)
 *     13
 */

fun countExchangeWays(amount: Int): Int 
{
    var count = 0
    // Iterate x from 1 to (amount - 3) / 5 (since 5x + 2*1 + 1*1 <= amount => 5x <= amount - 3)
    val maxX = (amount - 3) / 5
    for (x in 1..maxX) {
        // Remaining amount after using x 5-unit coins: amount - 5x
        val remainingAfterX = amount - 5 * x
        // Now, 2y + z = remainingAfterX, with y >=1, z >=1
        // So 2y <= remainingAfterX - 1 (since z >=1)
        val maxY = (remainingAfterX - 1) / 2
        for (y in 1..maxY) {
            val z = remainingAfterX - 2 * y
            if (z >= 1) {
                count++
            }
        }
    }
    return count
}
fun main(){

    check(countExchangeWays(50) == 106)
    check(countExchangeWays(20) == 13)
    check(countExchangeWays(15) == 6)
    check(countExchangeWays(12) == 3)

}

main()