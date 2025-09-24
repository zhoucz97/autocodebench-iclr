

object Solution {
    /**
     * Find all the numbers that are in the range [1, n] but are not present in the given array.
     *
     * @param nums An array containing n integers.
     * @return An array of numbers that are in the range [1, n] but are not present in the given array.
     *
     * Example:
     * findDisappearedNumbers(Array(4,3,2,7,8,2,3,1))
     * Output: Array(5,6)
     *
     * findDisappearedNumbers(Array(1,1))
     * Output: Array(2)
     */
  def findDisappearedNumbers(nums: Array[Int]): List[Int] = {
  val n = nums.length
  // Mark the presence of numbers by negating the value at the corresponding index
  for (num <- nums) {
    val index = math.abs(num) - 1
    if (nums(index) > 0) {
      nums(index) = -nums(index)
    }
  }
  // Collect the indices where the value is still positive (indicating the number is missing)
  (1 to n).filter(i => nums(i - 1) > 0).toList
}
  def main(args: Array[String]): Unit = {
    val nums1 = Array(4, 3, 2, 7, 8, 2, 3, 1)
    val result1 = findDisappearedNumbers(nums1)
    assert(result1.contains(5))
    assert(result1.contains(6))

    val nums2 = Array(1, 1)
    val result2 = findDisappearedNumbers(nums2)
    assert(result2.contains(2))

    val nums3 = Array(2, 2, 2, 2, 2)
    val result3 = findDisappearedNumbers(nums3)
    assert(result3.contains(1))
    assert(result3.contains(3))
    assert(result3.contains(4))
    assert(result3.contains(5))

    println("All tests passed")
  }
}