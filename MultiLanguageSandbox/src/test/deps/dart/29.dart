
int countArrangements(int n, int m, List<int> a)
/*Upon the grand opening of xl's flower shop, aiming to attract customers, he plans to arrange a row of flowers, totaling $m$ pots, at the entrance of his shop. Through surveying customer preferences, xl has identified the $n$ most favored types of flowers by customers, numbered from $1$ to $n$. To display a greater variety of flowers at the entrance, it is stipulated that the $i$-th type of flower cannot exceed $a_i$ pots, and when arranging, the same type of flower should be placed together, with different types of flowers arranged in ascending order of their numbers.
The task is to compute the total number of distinct flower arranging schemes.

Input Format
The first line contains two positive integers $n$ and $m$, separated by a space.
The second line contains $n$ integers, separated by spaces, sequentially representing $a_1, a_2, \cdots, a_n$.

Output Format
Output a single integer, representing the number of schemes. 

example
countArrangements(1,2,[2]) => 2
*/
{
  // Initialize a DP table where dp[i][j] represents the number of ways to arrange first i flowers using j pots
  List<List<int>> dp = List.generate(n + 1, (_) => List.filled(m + 1, 0));
  dp[0][0] = 1; // Base case: 0 flowers, 0 pots -> 1 way

  for (int i = 1; i <= n; i++) {
    for (int j = 0; j <= m; j++) {
      for (int k = 0; k <= a[i - 1] && k <= j; k++) {
        dp[i][j] += dp[i - 1][j - k];
      }
    }
  }

  return dp[n][m];
}
void testCountWays() {
  // Test case 1: Simple case
  assert(countArrangements(1, 2, [2]) == 1);

  // Test case 2: Two types, more combinations
  assert(countArrangements(2, 2, [1, 1]) == 1);

  // Test case 3: More complicated case
  assert(countArrangements(3, 3, [1, 2, 3]) == 6);
}

void main() {
  testCountWays();
}