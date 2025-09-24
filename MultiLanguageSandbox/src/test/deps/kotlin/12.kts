
/**
 * Generates and returns a list of integers within a specified range (inclusive of both ends) that are divisible by a given divisor.
 * 
 * Example:
 * >>> printDivisibleNumbersInRange(1, 10, 2)
 *     [2, 4, 6, 8, 10]
 * >>> printDivisibleNumbersInRange(3, 15, 3)
 *     [3, 6, 9, 12, 15]
 * >>> printDivisibleNumbersInRange(5, 20, 5)
 *     [5, 10, 15, 20]
 */

fun printDivisibleNumbersInRange(min: Int, max: Int, divisor: Int): List<Int> 
{
    return (min..max).filter { it % divisor == 0 }
}
fun main() {
    check(printDivisibleNumbersInRange(1, 10, 2) == listOf(2, 4, 6, 8, 10))
    check(printDivisibleNumbersInRange(3, 15, 3) == listOf(3, 6, 9, 12, 15))
    check(printDivisibleNumbersInRange(5, 20, 5) == listOf(5, 10, 15, 20))
    check(printDivisibleNumbersInRange(7, 21, 7) == listOf(7, 14, 21))
    check(printDivisibleNumbersInRange(10, 30, 10) == listOf(10, 20, 30))
}

main()