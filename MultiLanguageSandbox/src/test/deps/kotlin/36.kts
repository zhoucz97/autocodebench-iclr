

/**
 * Finds the most frequent n-grams (substrings of length n) in the given text and their frequency.
 * An n-gram is a contiguous sequence of n characters from the text.
 * 
 * Args:
 * n (Int): The length of each n-gram.
 * text (String): The text to analyze for n-grams.
 * 
 * Returns:
 * Pair<Int, List<String>>: A pair where the first element is the highest frequency of any n-gram,
 * and the second element is a list of n-grams with that frequency, in the order they first appear in the text.
 * If the highest frequency is not greater than 1, returns "NO" as the only element in the list.
 * 
 * Examples:
 * >>> findMostFrequentNGrams(3, "abcdefabcd")
 * Pair(2, listOf("abc", "bcd"))
 * 
 * >>> findMostFrequentNGrams(2, "abcabc")
 * Pair(2, listOf("ab", "bc"))
 * 
 * >>> findMostFrequentNGrams(4, "abcdefg")
 * Pair(1, listOf("NO"))
 */

fun findMostFrequentNGrams(n: Int, text: String): Pair<Int, List<String>> 
{
    if (n <= 0 || text.length < n) {
        return Pair(1, listOf("NO"))
    }

    val nGramFrequency = mutableMapOf<String, Int>()
    val nGramOrder = mutableListOf<String>()

    for (i in 0..text.length - n) {
        val nGram = text.substring(i, i + n)
        nGramFrequency[nGram] = nGramFrequency.getOrDefault(nGram, 0) + 1
        if (nGram !in nGramOrder) {
            nGramOrder.add(nGram)
        }
    }

    if (nGramFrequency.isEmpty()) {
        return Pair(1, listOf("NO"))
    }

    val maxFrequency = nGramFrequency.values.maxOrNull() ?: 1
    val mostFrequentNGrams = nGramOrder.filter { nGramFrequency[it] == maxFrequency }

    return if (maxFrequency > 1) {
        Pair(maxFrequency, mostFrequentNGrams)
    } else {
        Pair(1, listOf("NO"))
    }
}
fun main() {
    check(findMostFrequentNGrams(3, "abcdefabcd") == Pair(2, listOf("abc", "bcd")))
    check(findMostFrequentNGrams(2, "abcabc") == Pair(2, listOf("ab", "bc")))
    check(findMostFrequentNGrams(4, "abcdefg") == Pair(1, listOf("NO")))
    check(findMostFrequentNGrams(2, "abcdabcd") == Pair(2, listOf("ab", "bc", "cd")))
}


main()