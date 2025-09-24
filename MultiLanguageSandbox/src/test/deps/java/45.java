import java.util.Arrays;
public class Solution {

  /**
   * Find the smallest integer that can be formed by rearranging the digits of the given positive integer n, which is greater than n.
   *
   * @param n The input positive integer.
   * @return The smallest rearranged integer greater than n, or -1 if not possible.
   *
   * Example:
   * findSmallestInteger(12)
   * Output: 21
   */
  public static int findSmallestInteger(int n) 
{
        char[] digits = Integer.toString(n).toCharArray();
        int len = digits.length;
        int pivot = -1;
        
        // Step 1: Find the pivot
        for (int i = len - 1; i > 0; i--) {
            if (digits[i] > digits[i - 1]) {
                pivot = i - 1;
                break;
            }
        }
        
        if (pivot == -1) {
            return -1; // The digits are in descending order, no larger number possible
        }
        
        // Step 2: Find the smallest digit larger than digits[pivot] to the right of pivot
        int swapIndex = pivot + 1;
        for (int i = pivot + 1; i < len; i++) {
            if (digits[i] > digits[pivot] && digits[i] <= digits[swapIndex]) {
                swapIndex = i;
            }
        }
        
        // Step 3: Swap the pivot and swapIndex
        char temp = digits[pivot];
        digits[pivot] = digits[swapIndex];
        digits[swapIndex] = temp;
        
        // Step 4: Sort the digits to the right of pivot in ascending order
        Arrays.sort(digits, pivot + 1, len);
        
        // Convert back to integer
        try {
            long result = Long.parseLong(new String(digits));
            if (result > Integer.MAX_VALUE) {
                return -1;
            }
            return (int) result;
        } catch (NumberFormatException e) {
            return -1;
        }
    }
  public static void main(String[] args) {
    assert findSmallestInteger(123) == 132;
    assert findSmallestInteger(531) == -1;
    assert findSmallestInteger(2071) == 2107;

    System.out.println("All tests passed");
  }
}