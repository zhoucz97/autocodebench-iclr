
public class Solution {

  /**
   * Reverse the characters of each word in the given string while maintaining the original word order.
   *
   * @param s The input string containing words separated by spaces.
   * @return The string with characters of each word reversed.
   *
   * Example:
   * reverseWords("Mr Ding")
   * Output: "rM gniD"
   */
  public static String reverseWords(String s) 
{
    if (s == null || s.isEmpty()) {
        return s;
    }
    
    // Split the string into words based on spaces
    String[] words = s.split(" ");
    StringBuilder result = new StringBuilder();
    
    for (int i = 0; i < words.length; i++) {
        // Reverse each word
        String reversedWord = new StringBuilder(words[i]).reverse().toString();
        result.append(reversedWord);
        
        // Append a space after each word except the last one
        if (i < words.length - 1) {
            result.append(" ");
        }
    }
    
    return result.toString();
}
  public static void main(String[] args) {
    testReverseWords("The quick brown fox", "ehT kciuq nworb xof");
    testReverseWords("Hello World", "olleH dlroW");
    testReverseWords("a b c d e f", "a b c d e f");
    System.out.println("All tests passed");
  }
}