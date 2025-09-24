

/**
    * Finds M positive integers whose sum is N and their product is as large as possible.
    * Outputs the lexicographically smallest sequence of these M integers.
    * 
    * Example:
    * >>> maxProductPartition(6, 3)
    *     [2, 2, 2]
    * >>> maxProductPartition(8, 3)
    *     [2, 3, 3]
**/
fun maxProductPartition(N: Int, M: Int): List<Int> 
{
    if (M > N) {
        return List(M) { 1 } // But according to problem statement, M should be <= N since sum is N and all numbers are positive
    }
    val base = N / M
    val remainder = N % M
    val result = mutableListOf<Int>()
    for (i in 0 until M) {
        if (i < remainder) {
            result.add(base + 1)
        } else {
            result.add(base)
        }
    }
    return result.sorted()
}
fun main() {
    check(maxProductPartition(6, 3) == listOf(2, 2, 2))
    check(maxProductPartition(8, 3) == listOf(2, 3, 3))
    check(maxProductPartition(10, 2) == listOf(5, 5))
    check(maxProductPartition(7, 3) == listOf(2, 2, 3))
}

main()