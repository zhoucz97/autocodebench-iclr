

/**
 * Finds the n-th three-digit number that is a perfect square and has at least two identical digits.
 * The search is conducted in ascending order.
 *
 * Example:
 * >>> findNthSquareNumberWithDuplicateDigits(1)
 *     100
 * >>> findNthSquareNumberWithDuplicateDigits(2)
 *     121
 */

fun findNthSquareNumberWithDuplicateDigits(n: Int): Int 
{
    val result = mutableListOf<Int>()
    for (i in 10..31) {
        val square = i * i
        val digits = square.toString().toCharArray()
        val uniqueDigits = digits.toSet()
        if (digits.size != uniqueDigits.size) {
            result.add(square)
            if (result.size == n) {
                return square
            }
        }
    }
    throw IllegalArgumentException("Not enough numbers found")
}
fun main() {
    check(findNthSquareNumberWithDuplicateDigits(1) == 100)
    check(findNthSquareNumberWithDuplicateDigits(2) == 121)
    check(findNthSquareNumberWithDuplicateDigits(3) == 144)  // Example of another test case
    check(findNthSquareNumberWithDuplicateDigits(4) == 225)  // Example of another test case
}


main()