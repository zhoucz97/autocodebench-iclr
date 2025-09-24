
/**
 * Write a Kotlin function that takes a string as input (length not exceeding 80 characters)
 * and counts the number of words in it. Words in the input string are separated by spaces.
 *
 * Example:
 * >>> countWords("This is a book")
 * 4
 * >>> countWords("   Hello   World!   ")
 * 2
 * >>> countWords("A  string  with  multiple  spaces")
 * 5
 */

fun countWords(inputString: String): Int
{
    return inputString.trim().split("\\s+".toRegex()).filter { it.isNotEmpty() }.size
}
fun main() {
    // Test cases
    check(countWords("This is a book") == 4)
    check(countWords("   Hello   World!  World World ") == 3)
    check(countWords("A  string  with  multiple  spaces") == 5)
    check(countWords("") == 0) // Edge case: Empty string should have 0 words
    check(countWords("SingleWord") == 1) // Edge case: String with a single word
    check(countWords("a a a a a a a a a a a a") == 1) 
    check(countWords("a a a ab ab c c c c c c b a c") == 4) 
}


main()