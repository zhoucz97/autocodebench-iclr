
/**
    * Calculate the number of various denominations of currency required to make up a given amount.
    * The function takes an integer representing the total amount and returns a list of integers.
    * The list contains the number of 100, 50, 20, 10, 5, and 1 yuan notes needed, in that order.
    * The goal is to minimize the total number of notes by using larger denominations where possible.
    *
    * Example:
    * >>> calculateCurrency(735)
    * [7, 0, 1, 1, 1, 0]  // 7*100 + 1*20 + 1*10 + 1*5 = 735
    * >>> calculateCurrency(123)
    * [1, 0, 1, 0, 0, 3]  // 1*100 + 1*20 + 3*1 = 123
**/
fun calculateCurrency(amount: Int): List<Int> 
{
    val denominations = listOf(100, 50, 20, 10, 5, 1)
    val result = mutableListOf<Int>()
    var remainingAmount = amount
    
    for (denomination in denominations) {
        val count = remainingAmount / denomination
        result.add(count)
        remainingAmount %= denomination
    }
    
    return result
}
fun main() {
    check(calculateCurrency(735) == listOf(7, 0, 1, 1, 1, 0))
    check(calculateCurrency(123) == listOf(1, 0, 1, 0, 0, 3))
    check(calculateCurrency(980) == listOf(9, 1, 1, 1, 0, 0))
    check(calculateCurrency(50) == listOf(0, 1, 0, 0, 0, 0))
}

main()