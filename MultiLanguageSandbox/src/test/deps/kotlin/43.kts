
/**
 * Finds the length of the longest consecutive sequence in a given list of integers.
 * Consecutive sequence means numbers that come sequentially without any gap.
 * 
 * Example:
 * >>> longestConsecutiveSequence(listOf(1, 5, 6, 2, 3, 4, 5, 6, 8, 9))
 *     6
 * >>> longestConsecutiveSequence(listOf(10, 4, 20, 1, 3, 2))
 *     4
**/
fun longestConsecutiveSequence(numbers: List<Int>): Int 
{
    if (numbers.isEmpty()) return 0
    
    val numberSet = numbers.toSet()
    var maxLength = 0
    
    for (num in numberSet) {
        // Only check if it's the start of a sequence
        if (!numberSet.contains(num - 1)) {
            var currentNum = num
            var currentLength = 1
            
            while (numberSet.contains(currentNum + 1)) {
                currentNum++
                currentLength++
            }
            
            maxLength = maxOf(maxLength, currentLength)
        }
    }
    
    return maxLength
}
fun main() {
    check(longestConsecutiveSequence(listOf(1, 5, 6, 2, 3, 4, 5, 6, 8, 9)) == 6)
    check(longestConsecutiveSequence(listOf(10, 4, 20, 1, 3, 2)) == 4)
    check(longestConsecutiveSequence(listOf(1, 2, 3, 4, 5, 6)) == 6)
    check(longestConsecutiveSequence(listOf(100, 4, 200, 1, 3, 2)) == 4)
}


main()