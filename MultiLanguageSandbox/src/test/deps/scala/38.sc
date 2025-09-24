


object Solution {
/**
  * Count the number of integers with unique digits from 0 to 10^n.
  *
  * @param n The number of digits.
  * @return The count of integers with unique digits.
  *
  * Example:
  * countNumbersWithUniqueDigits(2)
  * Output: 91
  *
  * Explanation: The answer should be the count of all numbers between 0 and 100 (exclusive) that have unique digits.
  * The numbers 11, 22, 33, 44, 55, 66, 77, 88, and 99 should be excluded.
  *
  * countNumbersWithUniqueDigits(0)
  * Output: 1
  *
  * Explanation: When n is 0, there is only one number, which is 0 itself.
  */
  def countNumbersWithUniqueDigits(n: Int): Int = {
        if (n == 0) return 1
        var total = 10
        var uniqueDigits = 9
        var availableNumbers = 9
        for (i <- 2 to n) {
            uniqueDigits *= availableNumbers
            total += uniqueDigits
            availableNumbers -= 1
        }
        total
    }

  def main(args: Array[String]): Unit = {
    assert(countNumbersWithUniqueDigits(1) == 10, "Test case 1 failed")
    assert(countNumbersWithUniqueDigits(3) == 739, "Test case 2 failed")
    assert(countNumbersWithUniqueDigits(4) == 5275, "Test case 3 failed")

    println("All tests passed")
  }
}