

object Solution {
    /**
       * Calculate the maximum number of consecutive 1s in a binary array.
       *
       * @param nums The binary array.
       * @return The maximum number of consecutive 1s.
       *
       * Example:
       * findMaxConsecutiveOnes(Array(1, 1, 0, 1, 1, 1))
       * Output: 3
       */
  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
  var maxCount = 0
  var currentCount = 0

  for (num <- nums) {
    if (num == 1) {
      currentCount += 1
      if (currentCount > maxCount) {
        maxCount = currentCount
      }
    } else {
      currentCount = 0
    }
  }

  maxCount
}
  def main(args: Array[String]): Unit = {
    //test case 1
    assert(findMaxConsecutiveOnes(Array(1, 1, 0, 1, 1, 1, 0, 0, 1, 1)) == 3)
    
    //test case 2
    assert(findMaxConsecutiveOnes(Array(0, 0, 0)) == 0)
    
    //test case 3
    assert(findMaxConsecutiveOnes(Array(1, 1, 1, 1, 1)) == 5)

    println("All tests passed")
  }
}