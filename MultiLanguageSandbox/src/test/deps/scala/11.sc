


object Solution {
  /**
    * Calculate the total amount of water that can be trapped between the given heights.
    *
    * @param height An array representing the heights of each bar.
    * @return The total amount of water that can be trapped.
    *
    * Example:
    * trap(Array(0,1,0,2,1,0,1,3,2,1,2,1))
    * Output: 6
    *
    * Explanation:
    * The heights are represented by [0,1,0,2,1,0,1,3,2,1,2,1], and in this scenario, 6 units of water can be trapped (represented by the blue section).
    */
  def trap(height: Array[Int]): Int = {
    if (height.isEmpty) return 0
    
    val n = height.length
    val leftMax = new Array[Int](n)
    val rightMax = new Array[Int](n)
    
    // Fill leftMax array
    leftMax(0) = height(0)
    for (i <- 1 until n) {
        leftMax(i) = math.max(leftMax(i - 1), height(i))
    }
    
    // Fill rightMax array
    rightMax(n - 1) = height(n - 1)
    for (i <- (n - 2) to 0 by -1) {
        rightMax(i) = math.max(rightMax(i + 1), height(i))
    }
    
    // Calculate trapped water
    var trappedWater = 0
    for (i <- 0 until n) {
        trappedWater += math.min(leftMax(i), rightMax(i)) - height(i)
    }
    
    trappedWater
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(trap(Array(1, 0, 2, 0, 1)) == 2)
    assert(trap(Array(3, 2, 1, 0, 1, 2, 3)) == 9)
    assert(trap(Array(2, 1, 4, 3, 1, 2)) == 2)

    println("All tests passed")
  }
}
      