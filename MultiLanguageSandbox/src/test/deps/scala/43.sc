

import scala.collection.mutable.ArrayBuffer

object Solution {
  /**
    * Move all even elements to the front of the array, followed by all odd elements.
    *
    * @param nums An array of integers.
    * @return The modified array with even elements in the front and odd elements at the end.
    *
    * Example:
    * moveEvenOdd(Array(3, 1, 2, 4))
    * Output: Array(2, 4, 3, 1)
    *
    * Note: Other valid outputs include Array(4, 2, 3, 1), Array(2, 4, 1, 3), and Array(4, 2, 1, 3).
    */
  def moveEvenOdd(nums: Array[Int]): Array[Int] = {
  nums.partition(_ % 2 == 0) match {
    case (evens, odds) => evens ++ odds
  }
}
  def main(args: Array[String]): Unit = {
    // Test case 1
    assert(moveEvenOdd(Array(5, 8, 3, 6)).sameElements(Array(8, 6, 5, 3)))

    // Test case 2
    assert(moveEvenOdd(Array(7, 2)).sameElements(Array(2, 7)))

    // Test case 3
    assert(moveEvenOdd(Array(9, 1, 4, 7, 6)).sameElements(Array(4, 6, 9, 1, 7)))

    println("All tests passed")
  }
}