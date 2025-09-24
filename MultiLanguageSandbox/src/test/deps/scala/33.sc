import scala.collection.mutable.ArrayBuffer

object Solution {
  /**
     * Rearrange the numbers in the array to form the largest possible integer.
     *
     * @param nums An array of non-negative integers.
     * @return The largest possible integer formed by rearranging the numbers.
     *
     * Example:
     * largestNumber(Array(10, 2))
     * Output: "210"
     *
     * largestNumber(Array(3, 30, 34, 5, 9))
     * Output: "9534330"
     */
  def largestNumber(nums: Array[Int]): String = {
        val numStrings = nums.map(_.toString)
        
        // Custom comparator to decide the order of two strings
        implicit val ordering: Ordering[String] = new Ordering[String] {
            override def compare(a: String, b: String): Int = {
                val ab = a + b
                val ba = b + a
                ba.compareTo(ab)
            }
        }
        
        val sorted = numStrings.sorted
        
        // Handle the case where the largest number is "0" (e.g., input is [0, 0])
        if (sorted.head == "0") "0"
        else sorted.mkString
    }

  def main(args: Array[String]): Unit = {
    assert(largestNumber(Array(10, 2)) == "210")
    assert(largestNumber(Array(3, 30, 34, 5, 9)) == "9534330")
    // additional test cases 
    assert(largestNumber(Array(1, 34, 3, 98, 9, 76, 45, 4)) == "998764543431")
    assert(largestNumber(Array(54, 546, 548, 60)) == "6054854654")
    assert(largestNumber(Array(1, 34, 3, 98, 34, 3, 667, 66)) == "98667663434331")

    println("All tests passed")
  }
}