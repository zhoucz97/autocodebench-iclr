

object Solution {
  
  /**
   * Count the number of words in the given word list that have the given prefix.
   *
   * @param wordList a list of words
   * @param prefix the prefix string
   * @return the number of words with the given prefix
   *
   * Example:
   *
   * {{{
   * assert(countPrefixWords(List("banana", "band", "bee", "absolute", "acm"), "ba") == 2)
   * assert(countPrefixWords(List("banana", "band", "bee", "absolute", "acm"), "b") == 3)
   * assert(countPrefixWords(List("banana", "band", "bee", "absolute", "acm"), "band") == 1)
   * }}}
   */
  def countPrefixWords(wordList: List[String], prefix: String): Int = {
  wordList.count(word => word.startsWith(prefix))
}
  def main(args: Array[String]): Unit = {
    // Test the function with different test cases
    assert(countPrefixWords(List("apple", "ape", "april", "ant", "an"), "ap") == 3)
    assert(countPrefixWords(List("cat", "cap", "cape", "camp"), "ca") == 4)
    assert(countPrefixWords(List("dog", "dodge", "dot", "dough"), "do") == 4)
    println("All tests passed")
  }
}