


object Solution {
    /**
       * Move all the zeros to the end of the array while maintaining the relative order of the non-zero elements.
       *
       * @param nums An array of integers.
       * @return None (In-place modification of the input array).
       *
       * Example:
       * moveZeroes(Array(0, 1, 0, 3, 12))
       * Output: Array(1, 3, 12, 0, 0)
       */
  def moveZeroes(nums: Array[Int]): Unit = {
  var nonZeroIndex = 0
  // First pass: move all non-zero elements to the front
  for (i <- nums.indices) {
    if (nums(i) != 0) {
      nums(nonZeroIndex) = nums(i)
      nonZeroIndex += 1
    }
  }
  // Second pass: fill the remaining positions with zeros
  for (i <- nonZeroIndex until nums.length) {
    nums(i) = 0
  }
}
  def main(args: Array[String]): Unit = {
    val nums1 = Array(1,0,2,0,3)
    Solution.moveZeroes(nums1)
    assert(nums1 sameElements Array(1,2,3,0,0))

    val nums2 = Array(0,0,0,1,2)
    Solution.moveZeroes(nums2)
    assert(nums2 sameElements Array(1,2,0,0,0))

    val nums3 = Array(1,2,3,0,0)
    Solution.moveZeroes(nums3)
    assert(nums3 sameElements Array(1,2,3,0,0))

    println("All tests passed")
  }
}