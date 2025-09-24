
/**
 * Converts a number from one base to another.
 * The function takes an input number as a string (which can include digits and letters),
 * the base of this number, and the target base for conversion. The bases can range from 2 to 16.
 * The output is the converted number as a string in the target base, using uppercase letters for bases above 10.
 *
 * Examples:
 * >>> convertBase("Aab3", 15, 7)
 *     "210306"
 * >>> convertBase("101", 2, 10)
 *     "5"
 * >>> convertBase("FF", 16, 2)
 *     "11111111"
 */

fun convertBase(inputNumber: String, fromBase: Int, toBase: Int): String 
{
    require(fromBase in 2..16) { "fromBase must be between 2 and 16" }
    require(toBase in 2..16) { "toBase must be between 2 and 16" }

    // Convert the input number to decimal (base 10)
    var decimalValue = 0L
    for (char in inputNumber.toUpperCase()) {
        val digitValue = when (char) {
            in '0'..'9' -> char - '0'
            in 'A'..'F' -> 10 + (char - 'A')
            else -> throw IllegalArgumentException("Invalid character in input number")
        }
        if (digitValue >= fromBase) {
            throw IllegalArgumentException("Digit '$char' is not valid for base $fromBase")
        }
        decimalValue = decimalValue * fromBase + digitValue
    }

    // Handle the special case of 0
    if (decimalValue == 0L) return "0"

    // Convert the decimal value to the target base
    val result = StringBuilder()
    var value = decimalValue
    while (value > 0) {
        val remainder = value % toBase
        val digitChar = when (remainder) {
            in 0..9 -> ('0' + remainder).toString()
            in 10..15 -> ('A' + (remainder - 10)).toString()
            else -> throw IllegalStateException("Unexpected remainder value")
        }
        result.insert(0, digitChar)
        value /= toBase
    }

    return result.toString()
}
fun main() {
    check(convertBase("Aab3", 15, 7) == "210306")
    check(convertBase("101", 2, 10) == "5")
    check(convertBase("FF", 16, 2) == "11111111")
    check(convertBase("123", 10, 16) == "7B")
}

main()