

object Main extends App {

  /**
    * Check if in given list of numbers, any two numbers are closer to each other than
    * given threshold.
  */
  def hasCloseElements(numbers: List[Double], threshold: Double): Boolean ={
  // Sort the list to easily find adjacent elements
  val sortedNumbers = numbers.sorted
  
  // Check all adjacent pairs in the sorted list
  sortedNumbers.sliding(2).exists { case List(a, b) => 
    math.abs(a - b) < threshold
  }
}
// Test cases
  def test(): Unit = {
    assert(hasCloseElements(List(1.0, 2.0, 3.9, 4.0, 5.0, 2.2), 0.3) == true)
    assert(hasCloseElements(List(1.0, 2.0, 3.9, 4.0, 5.0, 2.2), 0.05) == false)
    assert(hasCloseElements(List(1.0, 2.0, 5.9, 4.0, 5.0), 0.95) == true)
    assert(hasCloseElements(List(1.0, 2.0, 5.9, 4.0, 5.0), 0.8) == false)
    assert(hasCloseElements(List(1.0, 2.0, 3.0, 4.0, 5.0, 2.0), 0.1) == true)
    assert(hasCloseElements(List(1.1, 2.2, 3.1, 4.1, 5.1), 1.0) == true)
    assert(hasCloseElements(List(1.1, 2.2, 3.1, 4.1, 5.1), 0.5) == false)
  }

  // Run the test
  test()
}