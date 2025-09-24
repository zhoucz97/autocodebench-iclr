
class LongestIncreasingPath {
    static int longestIncreasingPath(ArrayList matrix) {
        """
        Find the length of the longest increasing path in the given matrix.
        You can move in four directions: up, down, left, right.
        You are not allowed to move diagonally or outside the boundary.
        >>> longestIncreasingPath([[9,9,4],[6,6,8],[2,1,1]])
        4
        >>> longestIncreasingPath([[3,4,5],[3,2,6],[2,2,1]])
        4
        >>> longestIncreasingPath([[1]])
        1
        """

        if (matrix == null || matrix.size() == 0 || matrix[0].size() == 0) {
            return 0
        }
        int rows = matrix.size()
        int cols = matrix[0].size()
        int[][] dp = new int[rows][cols]
        int maxLen = 0
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                int len = dfs(matrix, i, j, dp)
                maxLen = Math.max(maxLen, len)
            }
        }
        return maxLen
    }
    
    private static int dfs(ArrayList<ArrayList<Integer>> matrix, int i, int j, int[][] dp) {
        if (dp[i][j] != 0) {
            return dp[i][j]
        }
        int rows = matrix.size()
        int cols = matrix[0].size()
        int[] directions = [ -1, 0, 1, 0, -1 ] // up, right, down, left
        int maxLen = 1
        
        for (int k = 0; k < 4; k++) {
            int x = i + directions[k]
            int y = j + directions[k + 1]
            if (x >= 0 && x < rows && y >= 0 && y < cols && matrix[x][y] > matrix[i][j]) {
                int len = 1 + dfs(matrix, x, y, dp)
                maxLen = Math.max(maxLen, len)
            }
        }
        dp[i][j] = maxLen
        return maxLen
    }
}
// Test cases
def longestIncreasingPath = new LongestIncreasingPath()
assert longestIncreasingPath.longestIncreasingPath([[9,9,4],[6,6,8],[2,1,1]]) == 4
assert longestIncreasingPath.longestIncreasingPath([[3,4,5],[3,2,6],[2,2,1]]) == 4
assert longestIncreasingPath.longestIncreasingPath([[1]]) == 1
assert longestIncreasingPath.longestIncreasingPath([[1,2,3],[8,9,4],[7,6,5]]) == 9
assert longestIncreasingPath.longestIncreasingPath([[1,10,2],[9,3,3],[8,4,5]]) == 4
assert longestIncreasingPath.longestIncreasingPath([[1,10,2],[9,3,3],[8,4,5],[7,6,6]]) == 6
assert longestIncreasingPath.longestIncreasingPath([[1,10,2,11],[9,3,3,12],[8,4,5,13],[7,6,6,14]]) == 6