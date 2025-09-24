
/**
    * Finds the longest word in a given simple English sentence. The sentence ends with a period,
    * and words are separated by spaces. It assumes no abbreviations or special forms are present.
    * If there are multiple longest words of the same length, the first one is returned.
    *
    * Example:
    * >>> findLongestWord("I am a student of Peking University.")
    *     "University"
    * >>> findLongestWord("Hello world.")
    *     "Hello"
**/

fun findLongestWord(sentence: String): String 
{
    // Remove the trailing period and split the sentence into words
    val words = sentence.dropLast(1).split(" ")
    
    // Find the longest word (first one if there are ties)
    var longestWord = ""
    for (word in words) {
        if (word.length > longestWord.length) {
            longestWord = word
        }
    }
    
    return longestWord
}
fun main() {
    check(findLongestWord("I am a student of Peking University.") == "University")
    check(findLongestWord("Hello world.") == "Hello")
    check(findLongestWord("Short sentences are sweet.") == "sentences")
    check(findLongestWord("Each word has its own length.") == "length")
}


main()