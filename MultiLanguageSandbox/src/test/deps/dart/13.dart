
int passBallGame(int n, int m)
/*Calculates the number of ways a ball can be passed among `n` students `m` times,
such that the ball ends up with student 1 after the last pass.

This function uses dynamic programming to solve the problem, where `dp[i][j]`
represents the number of ways to pass the ball `i` times and end up with student `j`.
The function initializes a 2D list `dp` with dimensions `(m + 1) x (n + 1)` to store
intermediate results. The base case is set where no passes are made and the ball starts
with student 1. The function then iterates over the number of passes and students to fill
the `dp` table with the count of pass sequences for each scenario. The final result is the
number of ways to pass the ball `m` times and return it to student 1.

Arguments:
  n (int): The total number of students among whom the ball is being passed.
  m (int): The total number of passes to be made.

Returns:
  int: The number of distinct ways to pass the ball `m` times such that it
  ends up with student 1.

Examples:
  passBallGame(3, 1) returns 0
  passBallGame(5, 0) returns 1
*/
{
  // Create a DP table with (m+1) rows and (n+1) columns
  List<List<int>> dp = List.generate(m + 1, (_) => List.filled(n + 1, 0));
  
  // Base case: 0 passes, ball is with student 1
  dp[0][1] = 1;
  
  for (int i = 1; i <= m; i++) {
    for (int j = 1; j <= n; j++) {
      // The ball can come from j-1 or j+1 (if they exist)
      if (j - 1 >= 1) {
        dp[i][j] += dp[i - 1][j - 1];
      }
      if (j + 1 <= n) {
        dp[i][j] += dp[i - 1][j + 1];
      }
    }
  }
  
  // Return the number of ways after m passes ending with student 1
  return dp[m][1];
}
void testPassBallGame() {
  // Test case 1: Small number of students and passes
  assert(passBallGame(3, 1) == 0, 'Test Case 1 Failed'); // There are two ways to pass the ball once among 3 students and end with student 1.

  // Test case 2: A scenario where no passes are made
  assert(passBallGame(5, 0) == 1, 'Test Case 2 Failed'); // There is only one way (no passes) to end up with student 1 when no passes are made.

  // Test case 3: A larger number of students and passes
  assert(passBallGame(4, 2) == 2, 'Test Case 3 Failed'); // There are two ways to pass the ball twice among 4 students and end with student 1.

  // Additional test cases can be added here as needed.

  print('All test cases passed!');
}

void main() {
  testPassBallGame();
}