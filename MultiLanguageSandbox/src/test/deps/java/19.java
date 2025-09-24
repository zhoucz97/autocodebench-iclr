import java.util.Arrays;
import java.util.*;

class Solution {
    /**
     * Check if it is possible to split the given integer array into two non-empty arrays
     * with the same average.
     *
     * >>> splitArraySameAverage(new int[]{1,2,3,4,5,6,7,8})
     * true
     *
     * >>> splitArraySameAverage(new int[]{3,1})
     * false
     */
    public boolean splitArraySameAverage(int[] nums) 
{
        int n = nums.length;
        int sum = 0;
        for (int num : nums) {
            sum += num;
        }
        
        // DP[i][j] represents whether we can select j numbers with sum i
        boolean[][] dp = new boolean[sum + 1][n + 1];
        dp[0][0] = true;
        
        for (int num : nums) {
            for (int i = sum; i >= num; i--) {
                for (int j = n - 1; j >= 1; j--) {
                    if (dp[i - num][j - 1]) {
                        dp[i][j] = true;
                    }
                }
            }
        }
        
        for (int k = 1; k < n; k++) {
            if (sum * k % n == 0 && dp[sum * k / n][k]) {
                return true;
            }
        }
        return false;
    }
public static void main(String[] args) {
        Solution solution = new Solution();

        // Test the function with different test cases
        assert solution.splitArraySameAverage(new int[]{1, 2, 3, 4, 5, 6, 7, 8}) == true;
        assert solution.splitArraySameAverage(new int[]{1, 2, 3, 4, 5, 6, 7, 9}) == false;
        assert solution.splitArraySameAverage(new int[]{1, 2, 3, 4, 5, 6, 7, 10}) == true;
        assert solution.splitArraySameAverage(new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true;
        System.out.println("All tests passed");
    }
}