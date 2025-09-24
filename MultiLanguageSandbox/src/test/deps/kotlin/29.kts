
/**
 * Determines whether a given number string is a cyclic number.
 * A cyclic number is one where multiplying it by any number from 1 to its length,
 * results in a permutation of the original number.
 *
 * Examples:
 * >>> isCyclicNumber("142857")
 *     1 // since 142857 is a cyclic number
 * >>> isCyclicNumber("123456")
 *     0 // since 123456 is not a cyclic number
 */

fun isCyclicNumber(numberString: String): Int 
{
    val n = numberString.length
    val originalDigits = numberString.toCharArray().sorted()
    
    for (i in 1..n) {
        val product = numberString.toLong() * i
        val productString = product.toString()
        
        // Check if the product has the same number of digits as the original
        if (productString.length != n) {
            return 0
        }
        
        val productDigits = productString.toCharArray().sorted()
        if (!originalDigits.contentEquals(productDigits)) {
            return 0
        }
    }
    return 1
}
fun main(){
    check(isCyclicNumber("142857") == 1)
    check(isCyclicNumber("123456") == 0)
    check(isCyclicNumber("076923") == 0)
    check(isCyclicNumber("1") == 1)
    check(isCyclicNumber("0") == 1)

}

main()