


object Solution {
    /**
    * Calculate the number of arguments passed to the function.
    *
    * @param args An array of arguments.
    * @return The number of arguments passed.
    *
    * Example:
    * argumentsLength(Array(5))
    * Output: 1
    */
  def argumentsLength(args: Array[Any]): Int = {
  args.length
}
  def main(args: Array[String]): Unit = {
    // Test Case 1
    val testCase1 = Array[Any]("Hello", "World")
    assert(argumentsLength(testCase1) == 2)

    // Test Case 2
    val testCase2 = Array[Any](1, 2, 3, 4, 5)
    assert(argumentsLength(testCase2) == 5)

    // Test Case 3
    val testCase3 = Array[Any](10)
    assert(argumentsLength(testCase3) == 1)

    println("All tests passed")
  }
}