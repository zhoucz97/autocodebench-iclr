
import java.util.*;

class Solution {
    /**
     * Return the lexicographically smallest string of length n and value k.
     * The value of a string is the sum of the positions of its characters in the alphabet (starting from 1).
     * Example:
     * >>> getSmallestString(3, 27)
     * "aay"
     * >>> getSmallestString(5, 73)
     * "aaszz"
     */
    public String getSmallestString(int n, int k) 
{
    char[] result = new char[n];
    for (int i = 0; i < n; i++) {
        result[i] = 'a'; // Initialize all characters to 'a'
    }
    k -= n; // Subtract the base value (n * 1) from k
    
    int pos = n - 1;
    while (k > 0) {
        int add = Math.min(25, k); // The maximum we can add is 25 (to turn 'a' into 'z')
        result[pos] += add;
        k -= add;
        pos--;
    }
    
    return new String(result);
}

    public static void main(String[] args) {
        Solution solution = new Solution();

        // Test the function with different test cases
        assert solution.getSmallestString(3, 27).equals("aay");
        assert solution.getSmallestString(5, 73).equals("aaszz");
        assert solution.getSmallestString(3, 30).equals("acz");
        System.out.println("All tests passed");
    }
}