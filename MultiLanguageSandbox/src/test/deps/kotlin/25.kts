
/**
    * Finds the mode (most frequent element) and its count in a given list of integers.
    * If there are multiple modes, the smallest one is returned.
    *
    * Example:
    * >>> findModeAndCount(listOf(1, 2, 2, 2, 3, 5))
    *     Pair(2, 3) // 2 is the mode and it appears 3 times
    * >>> findModeAndCount(listOf(1, 1, 2, 3, 3))
    *     Pair(1, 2) // Both 1 and 3 are modes but 1 is smaller
**/

fun findModeAndCount(numbers: List<Int>): Pair<Int, Int> 
{
    if (numbers.isEmpty()) return Pair(0, 0) // Handle empty list case
    
    val frequencyMap = mutableMapOf<Int, Int>()
    
    // Count the frequency of each number
    for (number in numbers) {
        frequencyMap[number] = frequencyMap.getOrDefault(number, 0) + 1
    }
    
    var maxCount = 0
    var mode = 0
    
    // Find the mode with the highest frequency, choosing the smallest number in case of ties
    for ((number, count) in frequencyMap) {
        if (count > maxCount || (count == maxCount && number < mode)) {
            maxCount = count
            mode = number
        }
    }
    
    return Pair(mode, maxCount)
}
fun main() {
    check(findModeAndCount(listOf(1, 2, 2, 2, 3, 5)) == Pair(2, 3))
    check(findModeAndCount(listOf(1, 1, 2, 3, 3)) == Pair(1, 2))
    check(findModeAndCount(listOf(4, 4, 4, 5, 5, 6, 6, 6)) == Pair(4, 3))
    check(findModeAndCount(listOf(7, 7, 8, 9, 9, 9)) == Pair(9, 3))
    check(findModeAndCount(listOf(7, 7, 8, 9, 9, 9, 1, 1, 1, 1, 1)) == Pair(1, 5))
}

main()