
/**
 * Finds the missing (discontinuous) and duplicate ID numbers from a list of lists of ID numbers.
 * IDs are expected to be continuous but due to an error, one is missing and another is duplicated.
 * 
 * Example:
 * >>> findMissingAndDuplicateIds(listOf(listOf(5, 6, 8, 11, 9), listOf(10, 12, 9)))
 *     Pair(7, 9)
 * 
 * Note:
 * - The function assumes ID numbers are positive integers and the list contains at least one duplicate and one missing ID.
 * - The discontinuous ID cannot be the minimum or maximum of the entire ID set.
 */
fun findMissingAndDuplicateIds(ids: List<List<Int>>): Pair<Int, Int> 
{
    val flattened = ids.flatten().sorted()
    val min = flattened.minOrNull() ?: 0
    val max = flattened.maxOrNull() ?: 0
    
    // Calculate the expected sum if no numbers were missing or duplicated
    val expectedSum = (min..max).sum()
    val actualSum = flattened.sum()
    
    // The difference between expected and actual sum gives (missing - duplicate)
    val diff = expectedSum - actualSum
    
    // To find the duplicate, we can use a frequency map
    val frequency = mutableMapOf<Int, Int>()
    var duplicate = 0
    for (id in flattened) {
        frequency[id] = frequency.getOrDefault(id, 0) + 1
        if (frequency[id] == 2) {
            duplicate = id
            break
        }
    }
    
    val missing = duplicate + diff
    
    return Pair(missing, duplicate)
}
fun main() {
    check(findMissingAndDuplicateIds(listOf(listOf(5, 6, 8, 11, 9), listOf(10, 12, 9))) == Pair(7, 9))
    // Add more tests to ensure robustness
    check(findMissingAndDuplicateIds(listOf(listOf(1, 2, 4,7), listOf(3, 5, 5))) == Pair(6, 5))
    check(findMissingAndDuplicateIds(listOf(listOf(10, 11,17, 12, 14), listOf(13, 15, 12))) == Pair(16, 12))
}


main()