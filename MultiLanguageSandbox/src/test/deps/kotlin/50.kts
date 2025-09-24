
/**
 * Counts the number of digits, lowercase letters, and uppercase letters in a given string of length 8.
 * 
 * @param s The input string of length 8.
 * @return A Triple of integers where the first element is the count of digits,
 * the second element is the count of lowercase letters, and the third element
 * is the count of uppercase letters in the string.
 * 
 * Example:
 * >>> countCharacters("yLOI2022")
 * Triple(4, 1, 3)
 * >>> countCharacters("IAKIOIOI")
 * Triple(0, 0, 8)
 * >>> countCharacters("1n2s0e1s")
 * Triple(4, 4, 0)
 */
fun countCharacters(s: String): Triple<Int, Int, Int> 
{
    require(s.length == 8) { "Input string must be of length 8" }
    
    var digitCount = 0
    var lowerCaseCount = 0
    var upperCaseCount = 0
    
    for (char in s) {
        when {
            char.isDigit() -> digitCount++
            char.isLowerCase() -> lowerCaseCount++
            char.isUpperCase() -> upperCaseCount++
        }
    }
    
    return Triple(digitCount, lowerCaseCount, upperCaseCount)
}
fun main() {
    check(countCharacters("yLOI2022") == Triple(4, 1, 3))
    check(countCharacters("IAKIOIOI") == Triple(0, 0, 8))
    check(countCharacters("1n2s0e1s") == Triple(4, 4, 0))
}


main()