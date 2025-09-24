import java.util.ArrayList;
import java.util.List;
import java.util.*;

class Solution {
    /**
     * Given an integer k, return the minimum number of Fibonacci numbers whose sum is equal to k.
     * Fibonacci numbers are defined as follows:
     * F1 = 1
     * F2 = 1
     * Fn = Fn-1 + Fn-2, for n > 2.
     * It is guaranteed that there is at least one valid solution for the given k.
     *
     * Example 1:
     * Input: k = 7
     * Output: 2
     * Explanation: Fibonacci numbers are: 1, 1, 2, 3, 5, 8, 13, ...
     * For k = 7, we can get 2 + 5 = 7.
     *
     * Example 2:
     * Input: k = 10
     * Output: 2
     * Explanation: For k = 10, we can get 2 + 8 = 10.
     *
     * Example 3:
     * Input: k = 19
     * Output: 3
     * Explanation: For k = 19, we can get 1 + 5 + 13 = 19.
     */
  public int findMinFibonacciNumbers(int k) 
{
        List<Integer> fibs = new ArrayList<>();
        fibs.add(1);
        fibs.add(1);
        
        // Generate Fibonacci numbers up to k
        while (true) {
            int nextFib = fibs.get(fibs.size() - 1) + fibs.get(fibs.size() - 2);
            if (nextFib > k) {
                break;
            }
            fibs.add(nextFib);
        }
        
        int count = 0;
        int remaining = k;
        // Iterate from the largest to smallest Fibonacci number
        for (int i = fibs.size() - 1; i >= 0; i--) {
            if (fibs.get(i) <= remaining) {
                remaining -= fibs.get(i);
                count++;
                if (remaining == 0) {
                    break;
                }
            }
        }
        
        return count;
    }
  public static void main(String[] args) {
    Solution solution = new Solution();

    // Test the function with different test cases
    assert solution.findMinFibonacciNumbers(7) == 2;
    assert solution.findMinFibonacciNumbers(10) == 2;
    assert solution.findMinFibonacciNumbers(13) == 1;
    System.out.println("All tests passed");
  }
}