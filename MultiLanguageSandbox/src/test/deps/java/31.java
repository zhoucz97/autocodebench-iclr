import java.util.HashSet;
import java.util.Set;
import java.util.*;

class Solution {

  /**
   * Given a string s, split the string and return the maximum number of unique substrings.
   * The split substrings should be able to reconstruct the original string, and each substring must be unique.
   *
   * Example:
   * Input: s = "ababccc"
   * Output: 5
   * Explanation: One possible way to split the string is ['a', 'b', 'ab', 'c', 'cc'].
   *              Splitting the string as ['a', 'b', 'a', 'b', 'c', 'cc'] is not valid, because both 'a' and 'b' appear more than once.
   *
   * Note:
   * - The length of s is between 1 and 16.
   * - s only contains lowercase English letters.
   */
  public int maxUniqueSplit(String s) 
{
        Set<String> used = new HashSet<>();
        return backtrack(s, 0, used);
    }
    
    private int backtrack(String s, int start, Set<String> used) {
        if (start == s.length()) {
            return 0;
        }
        int max = 0;
        for (int end = start + 1; end <= s.length(); end++) {
            String substring = s.substring(start, end);
            if (!used.contains(substring)) {
                used.add(substring);
                int res = 1 + backtrack(s, end, used);
                max = Math.max(max, res);
                used.remove(substring);
            }
        }
        return max;
    }
  public static void main(String[] args) {
    Solution solution = new Solution();

    // Test the function with different test cases
    assert solution.maxUniqueSplit("ababccc") == 5;
    assert solution.maxUniqueSplit("abcabcabc") == 6;
    assert solution.maxUniqueSplit("abcdefg") == 7;
    assert solution.maxUniqueSplit("aaaaaaa") == 3;
    System.out.println("All tests passed");
  }
}