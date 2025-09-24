
/**
 * Given a laboratory with N test tubes arranged in a straight line, you need to find the number of ways
 * to safely place chemicals in the test tubes. Placing chemicals in M consecutive test tubes will cause an explosion,
 * so you need to ensure that no M consecutive test tubes contain chemicals.
 *
 * Write a function that calculates the total number of safe placement options.
 *
 * Example:
 * >>> countSafePlacementOptions(4, 3)
 * 13
 *
 * Constraints:
 * 1 < N < 32
 * 2 <= M <= 5
 *
 * Note: The result can be a large number, so return it as a Long.
 */


fun countSafePlacementOptions(N: Int, M: Int): Long
{
    if (M == 1) return 0L // though constraints say M >= 2, but just in case
    
    val dp = Array(N + 1) { LongArray(M) }
    
    // Base cases:
    // For the first test tube:
    // 0 consecutive: not placing a chemical (1 way)
    // 1 consecutive: placing a chemical (1 way)
    dp[1][0] = 1
    dp[1][1] = 1
    
    for (i in 2..N) {
        for (j in 0 until M) {
            if (j == 0) {
                // If we don't place a chemical, the previous could have any j (0..M-1)
                var sum = 0L
                for (k in 0 until M) {
                    sum += dp[i - 1][k]
                }
                dp[i][j] = sum
            } else {
                // If we place a chemical, the previous must have j-1 consecutive
                dp[i][j] = dp[i - 1][j - 1]
            }
        }
    }
    
    var total = 0L
    for (j in 0 until M) {
        total += dp[N][j]
    }
    
    return total
}
fun main() {
    // Test cases
    check(countSafePlacementOptions(4, 3) == 13L)
    check(countSafePlacementOptions(3, 2) == 5L)
    check(countSafePlacementOptions(5, 2) == 13L)
    check(countSafePlacementOptions(7, 3) == 81L)
    check(countSafePlacementOptions(10, 4) == 773L)
    check(countSafePlacementOptions(12, 5) == 3525L)
}

main()