
class Solution {
    /**
    * Given a string s composed of '(' and ')', the left and right brackets in s may be unbalanced. Each operation can insert a '(' or ')', and calculate the minimum number of insertion operations required. Make the left and right brackets in s balanced.
    *
    * Example 1:
    * Input: s = "(()))"
    * Output: 1
    * Explanation:  We need to add one '(' at the first of the string to make it balanced: "((()))".
    * Example 2:
    * Input: s = "((()))"
    * Output: 0
    *
    * - s consists of '(' and ')' only.
    */
 public int minInsertions(String s) 
{
    int insertions = 0;
    int open = 0;
    
    for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i);
        if (c == '(') {
            open++;
        } else { // c == ')'
            if (open > 0) {
                open--;
            } else {
                insertions++; // insert a '(' before this ')'
            }
            // Check if the next character is also ')', which would require one insertion
            if (i + 1 < s.length() && s.charAt(i + 1) == ')') {
                i++; // skip the next ')'
            } else {
                insertions++; // insert a ')' to balance the current ')'
            }
        }
    }
    
    // Add remaining unmatched '('
    insertions += 2 * open;
    
    return insertions;
}
    public static void main(String[] args) {
        Solution solution = new Solution();

        // Test the function with different test cases
        assert solution.minInsertions("((()))") == 0;
        assert solution.minInsertions("()()()") == 0;
        assert solution.minInsertions("(()))(()") == 2;
        assert solution.minInsertions("))())(((") == 6;
        System.out.println("All tests passed");
    }
}