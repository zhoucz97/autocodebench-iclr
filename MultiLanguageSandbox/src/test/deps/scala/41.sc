


object Solution {
/**
  * Count the number of prime numbers less than a non-negative integer n.
  *
  * @param n The non-negative integer.
  * @return The number of prime numbers less than n.
  *
  * Example:
  * countPrimes(10)
  * Output: 4
  *
  * Explanation: There are 4 prime numbers less than 10, which are 2, 3, 5, and 7.
  */
  def countPrimes(n: Int): Int = {
  if (n <= 2) return 0
  
  val isPrime = Array.fill(n)(true)
  isPrime(0) = false
  isPrime(1) = false
  
  var i = 2
  while (i * i < n) {
    if (isPrime(i)) {
      var j = i * i
      while (j < n) {
        isPrime(j) = false
        j += i
      }
    }
    i += 1
  }
  
  isPrime.count(_ == true)
}
  def main(args: Array[String]): Unit = {
    // Additional test cases
    assert(countPrimes(15) == 6)
    assert(countPrimes(30) == 10)
    assert(countPrimes(50) == 15)

    println("All tests passed")
  }
}