
/**
 * Converts a given integer to its octal, decimal, and hexadecimal representations.
 * Args:
 *   number: An integer number to be converted.
 * Returns:
 *   A Triple containing the octal, decimal, and hexadecimal representations of the number.
 * Examples:
 *   >>> convertNumberToDifferentBases(10)
 *       Triple("12", "10", "A")
 *   >>> convertNumberToDifferentBases(255)
 *       Triple("377", "255", "FF")
 *   >>> convertNumberToDifferentBases(16)
 *       Triple("20", "16", "10")
 */

fun convertNumberToDifferentBases(number: Int): Triple<String, String, String> 
{
    val octal = number.toString(8) // Convert to octal (base 8)
    val decimal = number.toString(10) // Convert to decimal (base 10)
    val hexadecimal = number.toString(16).uppercase() // Convert to hexadecimal (base 16) and uppercase
    return Triple(octal, decimal, hexadecimal)
}
fun main() {
    check(convertNumberToDifferentBases(10) == Triple("12", "10", "A"))
    check(convertNumberToDifferentBases(255) == Triple("377", "255", "FF"))
    check(convertNumberToDifferentBases(16) == Triple("20", "16", "10"))
    check(convertNumberToDifferentBases(1) == Triple("1", "1", "1"))
    check(convertNumberToDifferentBases(0) == Triple("0", "0", "0"))
    check(convertNumberToDifferentBases(100) == Triple("144", "100", "64"))
    check(convertNumberToDifferentBases(500) == Triple("764", "500", "1F4"))
}

main()