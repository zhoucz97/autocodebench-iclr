
/**
 * Counts the number of students who have enrolled in both courses A and B.
 * The function takes two lists as arguments: the first list contains the IDs of students enrolled in course A,
 * and the second list contains the IDs of students enrolled in course B.
 * 
 * Example:
 * >>> countCommonStudents(listOf(1, 2, 3, 4, 5), listOf(1, 3, 4, 5, 6))
 *     4
 * >>> countCommonStudents(listOf(10, 11, 12), listOf(12, 13, 14))
 *     1
 */
fun countCommonStudents(aStudents: List<Int>, bStudents: List<Int>): Int 
{
    // Convert both lists to sets to find the intersection
    val setA = aStudents.toSet()
    val setB = bStudents.toSet()
    
    // Find the intersection of the two sets
    val commonStudents = setA.intersect(setB)
    
    // Return the size of the intersection set
    return commonStudents.size
}
fun main() {
    check(countCommonStudents(listOf(1, 2, 3, 4, 5), listOf(1, 3, 4, 5, 6)) == 4)
    check(countCommonStudents(listOf(10, 11, 12), listOf(12, 13, 14)) == 1)
    check(countCommonStudents(listOf(1, 2, 3), listOf(4, 5, 6)) == 0)
    check(countCommonStudents(listOf(1, 2, 3, 4), listOf(2, 4)) == 2)
}


main()