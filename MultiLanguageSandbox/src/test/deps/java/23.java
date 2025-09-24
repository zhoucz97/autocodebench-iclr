
class Solution {
    /**
    * Check if the searchWord is a prefix of any word in the sentence.
    * If searchWord is a prefix of multiple words, return the index of the first matching word.
    * If searchWord is not a prefix of any word, return -1.
    * 
    * Example:
    * >>> isPrefixOfWord("i love eating burger", "burg")
    * 4
    * >>> isPrefixOfWord("this problem is an easy problem", "pro")
    * 2
    * >>> isPrefixOfWord("i am tired", "you")
    * -1
    */
  public int isPrefixOfWord(String sentence, String searchWord) 
{
    String[] words = sentence.split(" ");
    for (int i = 0; i < words.length; i++) {
        if (words[i].startsWith(searchWord)) {
            return i + 1; // Return 1-based index
        }
    }
    return -1;
}
  public static void main(String[] args) {
      Solution solution = new Solution();

      // Test the function with different test cases
      assert solution.isPrefixOfWord("hello world", "wor") == 2;
      assert solution.isPrefixOfWord("hello world", "hell") == 1;
      assert solution.isPrefixOfWord("hello world", "foo") == -1;
      System.out.println("All tests passed");
  }
}