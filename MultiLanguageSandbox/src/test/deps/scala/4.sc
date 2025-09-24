import scala.collection.mutable.ArrayBuffer

import scala.collection.mutable.ArrayBuffer
object Solution {
    /**
    * Find the most competitive subsequence of length k from the given integer array.
    *
    * @param nums The integer array.
    * @param k The length of the subsequence.
    * @return The most competitive subsequence.
    *
    * Example:
    * mostCompetitive(Array(3, 5, 2, 6), 2)
    * Output: Array(2, 6)
    */
  def mostCompetitive(nums: Array[Int], k: Int): Array[Int] = {
    val stack = ArrayBuffer[Int]()
    for (i <- nums.indices) {
        val num = nums(i)
        while (stack.nonEmpty && num < stack.last && stack.length + (nums.length - i - 1) >= k) {
            stack.remove(stack.length - 1)
        }
        if (stack.length < k) {
            stack += num
        }
    }
    stack.toArray
}
        def main(args: Array[String]): Unit = {
            assert(mostCompetitive(Array(3, 5, 2, 6), 2).sameElements(Array(2, 6)))
            assert(mostCompetitive(Array(2, 4, 3, 3, 5, 4, 9, 6), 4).sameElements(Array(2, 3, 3, 4)))
            assert(mostCompetitive(Array(6, 7, 5, 3, 5, 6, 4, 2), 3).sameElements(Array(3, 4, 2)))
            println("All tests passed")
        }
    }