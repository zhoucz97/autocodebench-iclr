

object Solution {
  /**
    * Given an array of positive integers nums, return the maximum possible sum of an ascending subarray in nums.
    *
    * A subarray is defined as a contiguous sequence of numbers in an array.
    *
    * An ascending subarray is a subarray where the numbers are in strictly increasing order.
    *
    * Example:
    * maxAscendingSum(Array(10,20,30,5,10,50))
    * Output: 65
    */
  def maxAscendingSum(nums: Array[Int]): Int = {
  if (nums.isEmpty) return 0
  
  var maxSum = nums(0)
  var currentSum = nums(0)
  
  for (i <- 1 until nums.length) {
    if (nums(i) > nums(i - 1)) {
      currentSum += nums(i)
    } else {
      currentSum = nums(i)
    }
    
    if (currentSum > maxSum) {
      maxSum = currentSum
    }
  }
  
  maxSum
}
  def main(args: Array[String]): Unit = {
    assert(maxAscendingSum(Array(1, 2, 3, 4, 5, 6)) == 21)
    assert(maxAscendingSum(Array(10, 20, 30, 40, 50, 5, 6, 7, 8, 9)) == 150)
    assert(maxAscendingSum(Array(100, 200, 300, 400, 500, 1, 2, 3, 4, 5)) == 1500)
    println("All tests passed")
  }
}