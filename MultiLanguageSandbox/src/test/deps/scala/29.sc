

object Solution {
/**
 * Computes the minimum cost of merging N piles of sand into one pile. 
 * The cost of merging two adjacent piles is the sum of their sizes. 
 * The function aims to find a merge strategy that minimizes the total cost.
 *
 * @param sandPiles An array of integers representing the sizes of each sand pile.
 * @return The minimum total cost of merging all piles into one.
 *
 * Example:
 * minMergeCost(Array(1, 3, 5, 2))
 * Output: 22
 * 
 * Explanation:
 * Merging piles 1 and 2 first (cost = 4), then merging the result with pile 3 (cost = 9), 
 * and finally merging the last two piles (cost = 11), results in a total cost of 22.
 */

def minMergeCost(sandPiles: Array[Int]): Int = {
    val n = sandPiles.length
    if (n <= 1) return 0
    
    // Create a prefix sum array for quick sum calculations
    val prefixSum = new Array[Int](n + 1)
    for (i <- 0 until n) {
        prefixSum(i + 1) = prefixSum(i) + sandPiles(i)
    }
    
    // dp(i)(j) will store the minimal cost to merge piles i..j
    val dp = Array.ofDim[Int](n, n)
    
    // Fill the DP table for subarrays of length l (from 2 to n)
    for (l <- 2 to n) {  // l is the length of the subarray
        for (i <- 0 to n - l) {
            val j = i + l - 1
            dp(i)(j) = Int.MaxValue
            for (k <- i until j) {
                val cost = dp(i)(k) + dp(k + 1)(j) + (prefixSum(j + 1) - prefixSum(i))
                if (cost < dp(i)(j)) {
                    dp(i)(j) = cost
                }
            }
        }
    }
    
    dp(0)(n - 1)
}
  def main(args: Array[String]): Unit = {
    assert(minMergeCost(Array(1, 3, 5, 2)) == 22)
    assert(minMergeCost(Array(4, 3, 3)) == 16)
    assert(minMergeCost(Array(10, 20, 30)) == 90)
    assert(minMergeCost(Array(3, 4, 3, 9, 6, 7, 8, 10 )) == 147)
    println("All tests passed")
  }
}