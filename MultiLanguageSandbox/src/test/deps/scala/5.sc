import scala.collection.mutable
import scala.collection.mutable

import scala.collection.mutable.ListBuffer
object Solution {
    /**
      * Generate all possible palindrome strings by rearranging the characters in the input string.
      *
      * @param s The input string.
      * @return A list of all possible palindrome strings, without duplicates.
      *
      * Example:
      * generatePalindromes("aabb")
      * Output: List("abba", "baab")
      */
  def generatePalindromes(s: String): List[String] = {
    val charCounts = mutable.Map[Char, Int]().withDefaultValue(0)
    for (c <- s) {
        charCounts(c) += 1
    }
    
    var oddCount = 0
    var middleChar: Option[Char] = None
    val halfChars = mutable.ListBuffer[Char]()
    
    for ((c, count) <- charCounts) {
        if (count % 2 != 0) {
            oddCount += 1
            middleChar = Some(c)
        }
        halfChars ++= List.fill(count / 2)(c)
    }
    
    if (oddCount > 1) {
        return List.empty
    }
    
    // Generate all unique permutations of halfChars
    val uniquePermutations = halfChars.toList.permutations.toSet.toList
    
    uniquePermutations.map { perm =>
        val half = perm.mkString
        val reversedHalf = half.reverse
        middleChar match {
            case Some(c) => half + c + reversedHalf
            case None => half + reversedHalf
        }
    }
}
  def main(args: Array[String]): Unit = {
    // Test cases
    val result1 = generatePalindromes("aabb")
    assert(result1.contains("abba"))
    assert(result1.contains("baab"))
    
    val result2 = generatePalindromes("abc")
    assert(result2.isEmpty)

    val result3 = generatePalindromes("aabbc")
    assert(result3.contains("abcba"))
    assert(result3.contains("bacab"))
    
    println("All tests passed")
}
    
}