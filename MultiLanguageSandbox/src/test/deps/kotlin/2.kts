
/**
 * Finds all Armstrong numbers within a specified range.
 * An Armstrong number is a number that is equal to the sum of its own digits each raised to the power of the number of 3.
 * For example, 153 is an Armstrong number because 1^3 + 5^3 + 3^3 = 153.
    >>> findArmstrongNumbersInRange(1, 500)
        listOf(1, 153, 370, 371, 407)
    >>> findArmstrongNumbersInRange(500, 1000)
         emptyList<Int>()
 */

fun findArmstrongNumbersInRange(start: Int, end: Int): List<Int> 
{
    val armstrongNumbers = mutableListOf<Int>()
    
    for (number in start..end) {
        if (isArmstrongNumber(number)) {
            armstrongNumbers.add(number)
        }
    }
    
    return armstrongNumbers
}

private fun isArmstrongNumber(number: Int): Boolean {
    val digits = number.toString().map { it.toString().toInt() }
    val power = digits.size
    
    val sum = digits.sumOf { Math.pow(it.toDouble(), power.toDouble()).toInt() }
    
    return sum == number
}
fun main() {
    check(findArmstrongNumbersInRange(1, 500) == listOf(1, 153, 370, 371, 407))
    check(findArmstrongNumbersInRange(500, 1000) == emptyList<Int>())
    check(findArmstrongNumbersInRange(1, 100) == listOf(1))
    check(findArmstrongNumbersInRange(100, 200) == listOf(153))
    check(findArmstrongNumbersInRange(100, 1000) == listOf(153, 370, 371, 407))
}


main()