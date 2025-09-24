
public class Solution {

  /**
   * Given an integer n, add a dot ('.') as a thousand separator every three digits and return the result as a string.
   *
   * @param n The integer to add thousand separators to.
   * @return The string with thousand separators added.
   *
   * Example:
   * addThousandSeparator(987)
   * Output: "987"
   *
   * Example:
   * addThousandSeparator(1234)
   * Output: "1.234"
   *
   * Example:
   * addThousandSeparator(123456789)
   * Output: "123.456.789"
   *
   * Example:
   * addThousandSeparator(0)
   * Output: "0"
   */
  public static String addThousandSeparator(int n) 
{
    // Convert the number to a string
    String numStr = Integer.toString(n);
    
    // Handle the case where the number is 0
    if (numStr.equals("0")) {
        return "0";
    }
    
    StringBuilder result = new StringBuilder();
    int length = numStr.length();
    
    // Iterate from the end to the beginning of the string
    for (int i = 0; i < length; i++) {
        // Add a dot every three digits from the right
        if (i > 0 && (length - i) % 3 == 0) {
            result.append('.');
        }
        result.append(numStr.charAt(i));
    }
    
    return result.toString();
}
  public static void main(String[] args) {
    assert "2.147.483.647".equals(
        addThousandSeparator(2147483647)
      ) : "Test case 1 failed";
    assert "10.000".equals(addThousandSeparator(10000)) : "Test case 2 failed";
    assert "999".equals(addThousandSeparator(999)) : "Test case 3 failed";
    System.out.println("All tests passed");
  }
}