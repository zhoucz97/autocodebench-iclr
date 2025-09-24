

object Solution {
  /**
    * Check if the binary string contains at most one segment of consecutive '1's.
    *
    * @param s The input binary string.
    * @return True if the binary string contains at most one segment of consecutive '1's, false otherwise.
    *
    * Example:
    * checkOnesSegment("1001")
    * Output: false
    *
    * Example:
    * checkOnesSegment("110")
    * Output: true
    */
  def checkOnesSegment(s: String): Boolean = {
    var inSegment = false
    var segments = 0
    
    for (c <- s) {
        if (c == '1') {
            if (!inSegment) {
                segments += 1
                inSegment = true
            }
        } else {
            inSegment = false
        }
    }
    
    segments <= 1
}
  def main(args: Array[String]): Unit = {
    assert(checkOnesSegment("10101") == true)
    assert(checkOnesSegment("111000") == false)
    assert(checkOnesSegment("100111000") == false)
    println("All tests passed")
  }
}