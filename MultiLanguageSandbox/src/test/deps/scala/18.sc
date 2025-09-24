


object Solution {
  /**
    * Convert a non-negative integer to its English representation.
    *
    * @param num The number to be converted.
    * @return The English representation of the number.
    *
    * Example:
    * numberToWords(123)
    * Output: "One Hundred Twenty Three"
    *
    * numberToWords(12345)
    * Output: "Twelve Thousand Three Hundred Forty Five"
    *
    * numberToWords(1234567)
    * Output: "One Million Two Hundred Thirty Four Thousand Five Hundred Sixty Seven"
    */
  def numberToWords(num: Int): String = {
    if (num == 0) return "Zero"
    
    var n = num
    var result = ""
    var scaleIndex = 0
    
    while (n > 0) {
      val chunk = n % 1000
      if (chunk != 0) {
        val chunkStr = convertChunk(chunk)
        result = if (result.isEmpty) chunkStr 
                 else s"$chunkStr ${scales(scaleIndex)} $result"
      }
      n /= 1000
      scaleIndex += 1
    }
    
    result.trim.replaceAll("\\s+", " ")
  }
  
  private def convertChunk(chunk: Int): String = {
    var n = chunk
    var result = ""
    
    if (n >= 100) {
      val hundred = n / 100
      result = s"${units(hundred)} Hundred"
      n %= 100
    }
    
    if (n > 0) {
      if (result.nonEmpty) result += " "
      
      if (n < 10) {
        result += units(n)
      } else if (n < 20) {
        result += teens(n - 10)
      } else {
        val ten = n / 10
        val unit = n % 10
        result += tens(ten)
        if (unit > 0) {
          result += s" ${units(unit)}"
        }
      }
    }
    
    result
  }

  def main(args: Array[String]): Unit = {
    // Test cases
    assert(numberToWords(567) == "Five Hundred Sixty Seven")
    assert(numberToWords(123456) == "One Hundred Twenty Three Thousand Four Hundred Fifty Six")
    assert(numberToWords(987654321) == "Nine Hundred Eighty Seven Million Six Hundred Fifty Four Thousand Three Hundred Twenty One")

    println("All tests passed")
  }
}