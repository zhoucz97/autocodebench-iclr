
/**
 * Divide a large number (as a string) by 13 and return the quotient and remainder.
 * The input number can be up to 100 digits long.
 *
 * Example:
 * >>> divideByThirteen("2132104848488485")
 *   ("164008065268345", "0")
 * >>> divideByThirteen("169")
 *   ("13", "0")
 * >>> divideByThirteen("20")
 *   ("1", "7")
 */

fun divideByThirteen(number: String): Pair<String, String> 
{
    var remainder = 0
    val quotient = StringBuilder()
    
    for (digitChar in number) {
        val digit = digitChar - '0'
        val currentNumber = remainder * 10 + digit
        val quotientDigit = currentNumber / 13
        remainder = currentNumber % 13
        
        if (quotient.length > 0 || quotientDigit != 0) {
            quotient.append(quotientDigit)
        }
    }
    
    val quotientStr = if (quotient.isEmpty()) "0" else quotient.toString()
    return Pair(quotientStr, remainder.toString())
}
fun main() {
    check(divideByThirteen("2132104848488485") == Pair("164008065268345", "0"))
    check(divideByThirteen("169") == Pair("13", "0"))
    check(divideByThirteen("20") == Pair("1", "7"))
    check(divideByThirteen("130") == Pair("10", "0"))
    check(divideByThirteen("0") == Pair("0", "0"))
}


main()