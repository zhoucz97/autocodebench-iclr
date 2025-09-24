import scala.collection.mutable.ListBuffer

import scala.collection.mutable.ListBuffer

object Solution {
  /**
  * Generate all unique permutations of a sequence of numbers.
  *
  * @param nums The sequence of numbers.
  * @return A list of all unique permutations.
  *
  * Example:
  * permute(Array(1, 1, 2))
  * Output: List(List(1, 1, 2), List(1, 2, 1), List(2, 1, 1))
  */
  def permute(nums: Array[Int]): List[List[Int]] = {
  val result = ListBuffer[List[Int]]()
  
  def backtrack(start: Int): Unit = {
    if (start == nums.length - 1) {
      result += nums.toList
    } else {
      val seen = scala.collection.mutable.Set[Int]()
      for (i <- start until nums.length) {
        if (!seen.contains(nums(i))) {
          seen.add(nums(i))
          // Swap
          val temp = nums(start)
          nums(start) = nums(i)
          nums(i) = temp
          
          backtrack(start + 1)
          
          // Backtrack (swap back)
          val temp2 = nums(start)
          nums(start) = nums(i)
          nums(i) = temp2
        }
      }
    }
  }
  
  backtrack(0)
  result.toList
}
  def main(args: Array[String]): Unit = {
    // Test case 1
    assert(permute(Array(2, 3, 5)).toSet == Set(
      List(2, 3, 5), List(2, 5, 3), List(3, 2, 5),
      List(3, 5, 2), List(5, 2, 3), List(5, 3, 2)
    ))

    // Test case 2
    assert(permute(Array(0, -1, 1)).toSet == Set(
      List(0, -1, 1), List(0, 1, -1), List(-1, 0, 1),
      List(-1, 1, 0), List(1, 0, -1), List(1, -1, 0)
    ))

    // Test case 3
    assert(permute(Array(1, 2)).toSet == Set(
      List(1, 2), List(2, 1)
    ))

    println("All tests passed")
  }
}
