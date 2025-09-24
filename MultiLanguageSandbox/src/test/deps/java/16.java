import java.util.HashMap;
import java.util.Map;
import java.lang.*;
import java.util.*;

class Solution {
    /**
     * Check if it is possible to divide the array into n/2 pairs such that the sum of each pair is divisible by k.
     *
     * @param arr the input integer array
     * @param k the divisor
     * @return true if it is possible, false otherwise
     *
     * Example:
     * Input: arr = [1,2,3,4,5,10,6,7,8,9], k = 5
     * Output: true
     * Explanation: The pairs are (1,9), (2,8), (3,7), (4,6), (5,10), and the sum of each pair is divisible by 5.
     *
     * Input: arr = [1,2,3,4,5,6], k = 7
     * Output: true
     * Explanation: The pairs are (1,6), (2,5), (3,4), and the sum of each pair is divisible by 7.
     *
     * Input: arr = [1,2,3,4,5,6], k = 10
     * Output: false
     * Explanation: It is not possible to divide the array into n/2 pairs such that the sum of each pair is divisible by 10.
     */
  public boolean canArrange(int[] arr, int k) 
{
        Map<Integer, Integer> remainderCount = new HashMap<>();
        
        for (int num : arr) {
            int remainder = num % k;
            // Adjust negative remainders to be positive
            if (remainder < 0) {
                remainder += k;
            }
            remainderCount.put(remainder, remainderCount.getOrDefault(remainder, 0) + 1);
        }
        
        // Check for remainder 0: count must be even
        if (remainderCount.containsKey(0) && remainderCount.get(0) % 2 != 0) {
            return false;
        }
        
        // Check for other remainders
        for (int r = 1; r < k; r++) {
            int complement = k - r;
            if (remainderCount.containsKey(r)) {
                if (!remainderCount.containsKey(complement) || remainderCount.get(r) != remainderCount.get(complement)) {
                    return false;
                }
            } else {
                if (remainderCount.containsKey(complement)) {
                    return false;
                }
            }
        }
        
        return true;
    }
  public static void main(String[] args) {
    Solution solution = new Solution();

    // Test the function with different test cases
    assert solution.canArrange(new int[]{1,2,3,4,5,10,6,7,8,9}, 5) == true;
    assert solution.canArrange(new int[]{1,2,3,4,5,6}, 7) == true;
    assert solution.canArrange(new int[]{1,2,3,4,5,6}, 10) == false;
    System.out.println("All tests passed");
  }
}