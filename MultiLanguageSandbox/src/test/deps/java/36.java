import java.util.*;
import java.util.ArrayList;
import java.util.List;

public class Solution {

  /**
   * Find all stepping numbers in the range [low, high] where a stepping number is defined as a number where each digit is either one more or one less than the adjacent digit.
   *
   * @param low The lower bound of the range.
   * @param high The upper bound of the range.
   * @return A list of stepping numbers within the range [low, high] sorted in ascending order.
   *
   * Example:
   * findSteppingNumbers(0, 21)
   * Output: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 21]
   */
  public static List<Integer> findSteppingNumbers(int low, int high) 
{
        List<Integer> result = new ArrayList<>();
        
        // Handle the case where low is 0 separately
        if (low == 0) {
            result.add(0);
        }
        
        // Use BFS to generate stepping numbers
        Queue<Integer> queue = new LinkedList<>();
        // Start with digits 1-9 (since 0 is already handled)
        for (int i = 1; i <= 9; i++) {
            queue.offer(i);
        }
        
        while (!queue.isEmpty()) {
            int num = queue.poll();
            
            if (num > high) {
                continue;
            }
            
            if (num >= low) {
                result.add(num);
            }
            
            // Get the last digit of the current number
            int lastDigit = num % 10;
            
            // Generate next possible digits
            if (lastDigit > 0) {
                int nextNum = num * 10 + (lastDigit - 1);
                if (nextNum <= high) {
                    queue.offer(nextNum);
                }
            }
            if (lastDigit < 9) {
                int nextNum = num * 10 + (lastDigit + 1);
                if (nextNum <= high) {
                    queue.offer(nextNum);
                }
            }
        }
        
        Collections.sort(result);
        return result;
    }
  public static void main(String[] args) {
    assert findSteppingNumbers(0, 21)
      .equals(
        new ArrayList<>(List.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 21))
      );
    assert findSteppingNumbers(10, 15).equals(new ArrayList<>(List.of(10, 12)));
    assert findSteppingNumbers(50, 55).equals(List.of(54));

    System.out.println("All tests passed");
  }
}
