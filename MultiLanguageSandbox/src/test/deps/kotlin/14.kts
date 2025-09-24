
/**
 * Returns a list of all factors of the given number, excluding the number itself.
 * A factor is a number that divides the given number without leaving a remainder.
 * Examples:
 * >>> listFactors(10)
 * [1, 2, 5]
 * >>> listFactors(15)
 * [1, 3, 5]
 * >>> listFactors(7)
 * [1]
 */

fun listFactors(number: Int): List<Int> 
{
    if (number <= 1) return emptyList()
    
    val factors = mutableListOf<Int>()
    for (i in 1 until number) {
        if (number % i == 0) {
            factors.add(i)
        }
    }
    return factors
}
fun main(){
    check(listFactors(10) == listOf(1, 2, 5))
    check(listFactors(15) == listOf(1, 3, 5))
    check(listFactors(7) == listOf(1))
    check(listFactors(12) == listOf(1, 2, 3, 4, 6))
    check(listFactors(9) == listOf(1, 3))
    check(listFactors(17) == listOf(1))
    check(listFactors(20) == listOf(1, 2, 4, 5, 10))
}

main()