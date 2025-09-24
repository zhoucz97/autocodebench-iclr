import java.util.List;
import java.util.*;

class Solution {
    /**
     * Given a number of case-sensitive strings, finds the largest string X, such that either X, or its inverse can be found as a substring of any of the given strings.
     * 
     * Example:
     * >>> findLargestSubstring(Arrays.asList("ABCD", "BCDFF", "BRCD"))
     * 2
     * >>> findLargestSubstring(Arrays.asList("rose", "orchid"))
     * 2
     */
    public int findLargestSubstring(List<String> strings) 
{
        int maxLength = 0;
        
        for (String s : strings) {
            // Generate all possible substrings of s and their reverses
            for (int i = 0; i < s.length(); i++) {
                for (int j = i + 1; j <= s.length(); j++) {
                    String substring = s.substring(i, j);
                    String reversed = new StringBuilder(substring).reverse().toString();
                    
                    // Check if substring or its reverse exists in any string
                    if (isSubstringInAny(substring, strings) || isSubstringInAny(reversed, strings)) {
                        if (substring.length() > maxLength) {
                            maxLength = substring.length();
                        }
                    }
                }
            }
        }
        
        return maxLength;
    }
    
    private static boolean isSubstringInAny(String substring, List<String> strings) {
        for (String s : strings) {
            if (s.contains(substring)) {
                return true;
            }
        }
        return false;
    }
    public static void main(String[] args) {
        Solution solution = new Solution();

        // Test the function with different test cases
        assert solution.findLargestSubstring(Arrays.asList("ABCD", "BCDFF", "BRCD")) == 2;
        assert solution.findLargestSubstring(Arrays.asList("rose", "orchid")) == 2;
        assert solution.findLargestSubstring(Arrays.asList("hello", "world")) == 1;
        System.out.println("All tests passed");
    }
}