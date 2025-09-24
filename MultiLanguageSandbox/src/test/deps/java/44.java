import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;

public class Solution {

  /**
   * Return all elements in the matrix in diagonal order.
   *
   * @param mat The input matrix.
   * @return An array containing all elements in diagonal order.
   *
   * Example:
   * diagonalOrder(new int[][]{{1,2,3},{4,5,6},{7,8,9}})
   * Output: [1,2,4,7,5,3,6,8,9]
   */
  public static int[] diagonalOrder(int[][] mat) 
{
        if (mat == null || mat.length == 0 || mat[0].length == 0) {
            return new int[0];
        }
        
        int m = mat.length;
        int n = mat[0].length;
        Map<Integer, List<Integer>> map = new HashMap<>();
        
        // Group elements by their diagonal sum (i + j)
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                int sum = i + j;
                map.computeIfAbsent(sum, k -> new ArrayList<>()).add(mat[i][j]);
            }
        }
        
        List<Integer> result = new ArrayList<>();
        int maxSum = (m - 1) + (n - 1);
        
        for (int sum = 0; sum <= maxSum; sum++) {
            List<Integer> diagonal = map.get(sum);
            if (sum % 2 == 0) {
                // Reverse the list for even sums
                for (int i = diagonal.size() - 1; i >= 0; i--) {
                    result.add(diagonal.get(i));
                }
            } else {
                // Add in original order for odd sums
                result.addAll(diagonal);
            }
        }
        
        return result.stream().mapToInt(i -> i).toArray();
    }
  public static void main(String[] args) {
    assert java.util.Arrays.equals(
      diagonalOrder(new int[][] { { 1, 2 }, { 3, 4 } }),
      new int[] { 1, 2, 3, 4 }
    );
    assert java.util.Arrays.equals(
      diagonalOrder(
        new int[][] { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 9, 10, 11, 12 } }
      ),
      new int[] { 1, 2, 5, 9, 6, 3, 4, 7, 10, 11, 8, 12 }
    );
    assert java.util.Arrays.equals(
      diagonalOrder(new int[][] { { 1 }, { 2 }, { 3 } }),
      new int[] { 1, 2, 3 }
    );

    System.out.println("All tests passed");
  }
}