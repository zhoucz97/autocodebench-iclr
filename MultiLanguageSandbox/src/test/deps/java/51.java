import java.util.HashSet;
import java.util.Set;
import java.util.HashMap;

public class Solution {

  /**
   * Calculate the sum of unique elements in the given integer array.
   *
   * @param nums An array of integers.
   * @return The sum of unique elements.
   *
   * Example:
   * calculateUniqueSum(new int[]{1, 2, 3, 2})
   * Output: 4
   */
  public static int calculateUniqueSum(int[] nums) 
{
        Set<Integer> uniqueElements = new HashSet<>();
        
        for (int num : nums) {
            uniqueElements.add(num);
        }
        
        int sum = 0;
        for (int num : uniqueElements) {
            sum += num;
        }
        
        return sum;
    }
  public static void main(String[] args) {
    assert calculateUniqueSum(new int[] { 1, 4, 5, 1, 2, 4 }) == 7; // Test Case 1
    assert calculateUniqueSum(new int[] { 0, 0, 0, 0 }) == 0; // Test Case 2
    assert calculateUniqueSum(new int[] { -1, -2, -3, -2 }) == -4; // Test Case 3

    System.out.println("All tests passed");
  }
}