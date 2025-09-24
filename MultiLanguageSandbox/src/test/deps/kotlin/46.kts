
/**
 * Calculates the number of characters in the essay title, excluding spaces and newline characters.
 * 
 * Example:
 * >>> countTitleCharacters("234")
 *     3
 * >>> countTitleCharacters("Ca 45")
 *     4
 * 
 * The function counts uppercase and lowercase English letters, digits, but ignores spaces and newline characters.
 */
fun countTitleCharacters(title: String): Int 
{
    return title.count { it != ' ' && it != '\n' }
}
fun main() {
    check(countTitleCharacters("234") == 3)
    check(countTitleCharacters("Ca\n45") == 4)
    check(countTitleCharacters("Hello, World!") == 12) // Assuming "," is considered a valid character
    check(countTitleCharacters("A B C D E") == 5)
    check(countTitleCharacters("\n\n\n\n") == 0)
}


main()