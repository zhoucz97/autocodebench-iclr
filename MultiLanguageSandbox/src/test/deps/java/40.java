
public class Solution {

  /**
   * Given a binary matrix mat of size m x n, return the length of the longest continuous line of 1s in the matrix.
   * The line could be horizontal, vertical, diagonal, or anti-diagonal.
   *
   * @param mat The binary matrix.
   * @return The length of the longest continuous line of 1s.
   *
   * Example:
   * longestLine(new int[][]{{0,1,1,0},{0,1,1,0},{0,0,0,1}})
   * Output: 3
   */
  public static int longestLine(int[][] mat) 
{
    if (mat == null || mat.length == 0 || mat[0].length == 0) {
        return 0;
    }
    
    int m = mat.length;
    int n = mat[0].length;
    int maxLen = 0;
    
    // dp[i][j][0] : horizontal
    // dp[i][j][1] : vertical
    // dp[i][j][2] : diagonal (top-left to bottom-right)
    // dp[i][j][3] : anti-diagonal (top-right to bottom-left)
    int[][][] dp = new int[m][n][4];
    
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            if (mat[i][j] == 1) {
                // Horizontal: check left
                dp[i][j][0] = (j > 0) ? dp[i][j-1][0] + 1 : 1;
                // Vertical: check top
                dp[i][j][1] = (i > 0) ? dp[i-1][j][1] + 1 : 1;
                // Diagonal: check top-left
                dp[i][j][2] = (i > 0 && j > 0) ? dp[i-1][j-1][2] + 1 : 1;
                // Anti-diagonal: check top-right
                dp[i][j][3] = (i > 0 && j < n - 1) ? dp[i-1][j+1][3] + 1 : 1;
                
                // Update maxLen
                maxLen = Math.max(maxLen, Math.max(Math.max(dp[i][j][0], dp[i][j][1]), 
                                                  Math.max(dp[i][j][2], dp[i][j][3])));
            } else {
                // Reset all directions to 0 if current cell is 0
                dp[i][j][0] = 0;
                dp[i][j][1] = 0;
                dp[i][j][2] = 0;
                dp[i][j][3] = 0;
            }
        }
    }
    
    return maxLen;
}
  public static void main(String[] args) {
    assert longestLine(
      new int[][] {
        { 1, 1, 0, 0 },
        { 0, 1, 1, 0 },
        { 0, 0, 0, 1 },
        { 1, 0, 0, 0 },
      }
    ) ==
    3;
    assert longestLine(
      new int[][] {
        { 1, 0, 0, 1 },
        { 0, 1, 1, 0 },
        { 1, 1, 0, 0 },
        { 1, 0, 1, 1 },
      }
    ) ==
    4;
    assert longestLine(new int[][] { { 0, 0, 0 }, { 0, 1, 0 }, { 0, 0, 0 } }) ==
    1;

    System.out.println("All tests passed");
  }
}