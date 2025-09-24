import java.util.Arrays;
import java.lang.Math;

public class Solution {

  /**
   * Find and return the nth ugly number.
   *
   * @param n The index of the ugly number to find.
   * @return The nth ugly number.
   *
   * Example:
   * findNthUglyNumber(10)
   * Output: 12
   */
  public static int findNthUglyNumber(int n) 
{
        if (n <= 0) {
            return 0;
        }
        
        int[] uglyNumbers = new int[n];
        uglyNumbers[0] = 1; // The first ugly number is 1
        
        int i2 = 0, i3 = 0, i5 = 0;
        
        for (int i = 1; i < n; i++) {
            int nextMultipleOf2 = uglyNumbers[i2] * 2;
            int nextMultipleOf3 = uglyNumbers[i3] * 3;
            int nextMultipleOf5 = uglyNumbers[i5] * 5;
            
            int nextUgly = Math.min(nextMultipleOf2, Math.min(nextMultipleOf3, nextMultipleOf5));
            uglyNumbers[i] = nextUgly;
            
            // Move the pointer(s) that contributed to the next ugly number
            if (nextUgly == nextMultipleOf2) {
                i2++;
            }
            if (nextUgly == nextMultipleOf3) {
                i3++;
            }
            if (nextUgly == nextMultipleOf5) {
                i5++;
            }
        }
        
        return uglyNumbers[n - 1];
    }
  public static void main(String[] args) {
    assert findNthUglyNumber(1) == 1 : "Test case 1 failed";
    assert findNthUglyNumber(7) == 8 : "Test case 2 failed";
    assert findNthUglyNumber(15) == 24 : "Test case 3 failed";

    System.out.println("All tests passed");
  }
}
