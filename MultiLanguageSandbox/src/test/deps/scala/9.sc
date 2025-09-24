

object Solution {
  /**
    * Given an integer n, returns an array consisting of n unique integers that add up to 0.
    *
    * @param n The number of integers in the array.
    * @return An array of n unique integers that add up to 0.
    *
    * Example:
    * sumZero(5)
    * Output: [-7, -1, 1, 3, 4]
    *
    * Note:
    * Other valid arrays: [-5, -1, 1, 2, 3], [-3, -1, 2, -2, 4]
    */
  def sumZero(n: Int): Array[Int] = {
    val result = new Array[Int](n)
    var sum = 0
    for (i <- 0 until n - 1) {
        result(i) = i + 1
        sum += result(i)
    }
    result(n - 1) = -sum
    result
}
  
  def main(args: Array[String]): Unit = {
    // Test case 1
    val result1 = sumZero(4)
    assert(result1.sameElements(Array(1, 2, 3, -6)))
    
    // Test case 2
    val result2 = sumZero(6)
    assert(result2.sameElements(Array(1, 2, 3, 4, 5, -15)))
    
    // Test case 3
    val result3 = sumZero(8)
    assert(result3.sameElements(Array(1, 2, 3, 4, 5, 6, 7, -28)))
    
    println("All tests passed")
  }
}
