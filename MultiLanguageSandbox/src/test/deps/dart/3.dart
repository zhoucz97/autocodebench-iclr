
import 'dart:io';
int maxSumInGrid(List<List<int>> grid, int N)
/*Calculates the maximum sum of numbers that can be picked on a grid by two paths.

This function takes a square grid of integers and the size of the grid (N),
and computes the maximum sum of numbers that can be collected by two paths
starting from the top-left corner and ending at the bottom-right corner.
Each path can only move right or down, and no two paths can overlap.

Args:
  grid (List<List<int>>): A 2D list of integers representing the grid.
  N (int): The size of the dimensions of the square grid.

Returns:
  int: The maximum sum of numbers picked by the two paths.

Example:
  maxSumInGrid([
    [0, 0, 0, 0],
    [0, 4, 0, 0],
    [0, 0, 2, 0],
    [1, 0, 0, 0]
  ], 4);
  return 7
*/
{
  // Create a 3D DP table: dp[i1][j1][i2] represents the maximum sum when
  // path1 is at (i1, j1) and path2 is at (i2, j2), where j2 = (i1 + j1) - i2
  // because both paths have taken the same number of steps (i1 + j1 steps).
  // The total steps for both paths combined is 2*(N-1), but each path takes N-1 steps.
  var dp = List.generate(N, (_) => 
    List.generate(N, (_) => 
      List.generate(N, (_) => -1)
    )
  );

  // Initialize the starting position (0,0) for both paths
  dp[0][0][0] = grid[0][0];

  for (int i1 = 0; i1 < N; i1++) {
    for (int j1 = 0; j1 < N; j1++) {
      for (int i2 = 0; i2 < N; i2++) {
        int j2 = (i1 + j1) - i2;
        if (j2 < 0 || j2 >= N) continue;

        if (dp[i1][j1][i2] == -1) continue;

        // Generate next positions for both paths
        List<Point> path1Moves = [];
        if (i1 + 1 < N) path1Moves.add(Point(i1 + 1, j1));
        if (j1 + 1 < N) path1Moves.add(Point(i1, j1 + 1));

        List<Point> path2Moves = [];
        if (i2 + 1 < N) path2Moves.add(Point(i2 + 1, j2));
        if (j2 + 1 < N) path2Moves.add(Point(i2, j2 + 1));

        for (Point p1 in path1Moves) {
          for (Point p2 in path2Moves) {
            int ni1 = p1.x;
            int nj1 = p1.y;
            int ni2 = p2.x;
            int nj2 = p2.y;

            if (ni1 >= N || nj1 >= N || ni2 >= N || nj2 >= N) continue;

            // Check if the new positions are the same (paths overlap)
            if (ni1 == ni2 && nj1 == nj2) {
              // Only add the cell once
              int newSum = dp[i1][j1][i2] + grid[ni1][nj1];
              if (newSum > dp[ni1][nj1][ni2]) {
                dp[ni1][nj1][ni2] = newSum;
              }
            } else {
              // Add both cells' values
              int newSum = dp[i1][j1][i2] + grid[ni1][nj1] + grid[ni2][nj2];
              if (newSum > dp[ni1][nj1][ni2]) {
                dp[ni1][nj1][ni2] = newSum;
              }
            }
          }
        }
      }
    }
  }

  return dp[N-1][N-1][N-1];
}

class Point {
  int x, y;
  Point(this.x, this.y);
}
void testMaxSumInGrid() {
  // TestCase Structure
  List<Map<String, dynamic>> testCases = [
    {
      'grid': [
        [0, 0, 0, 0],
        [0, 4, 0, 0],
        [0, 0, 2, 0],
        [1, 0, 0, 0]
      ],
      'N': 4,
      'expected': 16
    },
    {
      'grid': [
        [0, 0, 13, 0],
        [0, 0, 0, 6],
        [0, 7, 0, 0],
        [0, 0, 0, 0]
      ],
      'N': 4,
      'expected': 58
    },
    // Add more test cases here
  ];

  for (var testCase in testCases) {
    int result = maxSumInGrid(testCase['grid'], testCase['N']);
    assert(result == testCase['expected'], "Test case failed: Expected ${testCase['expected']}, got $result");
    print('Test case passed: Expected ${testCase['expected']}, got $result');
  }
}

void main() {
  testMaxSumInGrid();
  // You can also handle the interactive part here, if necessary.
}