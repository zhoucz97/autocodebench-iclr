import java.util.ArrayList;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Solution {

  /**
   * Given an integer n, return all the center symmetric numbers of length n.
   *
   * @param n The length of the center symmetric numbers.
   * @return A list of center symmetric numbers.
   *
   * Example:
   * centerSymmetricNumbers(2)
   * Output: ["11","69","88","96"]
   *
   * centerSymmetricNumbers(1)
   * Output: ["0","1","8"]
   */
  public static List<String> centerSymmetricNumbers(int n) 
{'0', '1', ' ', ' ', ' ', ' ', '9', ' ', '8', '6'};
    
    public static List<String> centerSymmetricNumbers(int n) {
        List<String> result = new ArrayList<>();
        if (n == 0) {
            return result;
        }
        char[] current = new char[n];
        backtrack(current, 0, n - 1, result);
        return result;
    }
    
    private static void backtrack(char[] current, int left, int right, List<String> result) {
        if (left > right) {
            result.add(new String(current));
            return;
        }
        
        for (int i = 0; i < 10; i++) {
            char leftChar = (char) ('0' + i);
            char rightChar = MIRROR[i];
            
            if (rightChar == ' ') {
                continue; // invalid digit for center symmetric
            }
            
            if (left == right) {
                // middle digit for odd length
                if (i == 0 && left == 0) {
                    continue; // leading zero not allowed
                }
                if (i == 0 || i == 1 || i == 8) {
                    current[left] = leftChar;
                    backtrack(current, left + 1, right - 1, result);
                }
            } else {
                if (left == 0 && i == 0) {
                    continue; // leading zero not allowed
                }
                current[left] = leftChar;
                current[right] = rightChar;
                backtrack(current, left + 1, right - 1, result);
            }
        }
    }
  private static void assertArrays(String[] expected, List<String> actual) {
    if (actual == null || actual.size() != expected.length) {
      throw new AssertionError("Test case failed");
    }
    for (String str : expected) {
      if (!actual.contains(str)) {
        throw new AssertionError("Test case failed");
      }
    }
  }

  public static void main(String[] args) {
    assertArrays(
      new String[] { "11", "69", "88", "96" },
      centerSymmetricNumbers(2)
    );
    assertArrays(new String[] { "0", "1", "8" }, centerSymmetricNumbers(1));
    assertArrays(
      new String[] {
        "101",
        "111",
        "181",
        "609",
        "619",
        "689",
        "808",
        "818",
        "888",
        "906",
        "916",
        "986",
      },
      centerSymmetricNumbers(3)
    );
    System.out.println("All tests passed");
  }
}
