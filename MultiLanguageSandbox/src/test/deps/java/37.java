
public class Solution {

  /**
   * Calculate the number of times the digit 1 appears in all non-negative integers less than or equal to n.
   *
   * @param n An integer representing the upper limit.
   * @return The number of times the digit 1 appears.
   *
   * Example:
   * countDigitOne(13)
   * Output: 6
   *
   * Example:
   * countDigitOne(0)
   * Output: 0
   */
  public static int countDigitOne(int n) 
{
    int count = 0;
    long factor = 1; // current digit position (1, 10, 100, etc.)
    
    while (factor <= n) {
        long higher = n / (factor * 10);
        long current = (n / factor) % 10;
        long lower = n % factor;
        
        if (current == 0) {
            count += higher * factor;
        } else if (current == 1) {
            count += higher * factor + lower + 1;
        } else {
            count += (higher + 1) * factor;
        }
        
        factor *= 10;
    }
    
    return count;
}
  public static void main(String[] args) {
    assert countDigitOne(20) == 12 : "Test case 1 failed";
    assert countDigitOne(55) == 16 : "Test case 2 failed";
    assert countDigitOne(99) == 20 : "Test case 3 failed";
    System.out.println("All tests passed");
  }
}