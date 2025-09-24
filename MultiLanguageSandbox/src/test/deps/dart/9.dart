
int maxProduct(String numbers, int n, int k)
/*Calculates the maximum product by inserting `k` multiplication signs into
the given string of digits `numbers`.

This function utilizes dynamic programming to find the maximum product that
can be obtained by inserting `k` multiplication signs (`*`) into the string
`numbers`, which is composed of `n` digits. The algorithm computes this by
breaking down the problem into subproblems and storing intermediate results
in a two-dimensional list `dp`, where dp[i][j] represents the maximum
product obtainable from the first `i` digits with `j` multiplication signs.

Args:
  numbers (String): The string of digits to insert multiplication signs into.
  n (int): The number of digits in the `numbers` string.
  k (int): The number of multiplication signs to insert.

Returns:
  int: The maximum product that can be achieved by inserting `k`
       multiplication signs into the string `numbers`.

Examples:
  maxProduct("123", 3, 1) returns 46
  maxProduct("1234", 4, 2) returns 120
*/
{
  // Convert the string into a list of integers for easier access
  List<int> digits = numbers.split('').map((c) => int.parse(c)).toList();
  
  // Initialize a DP table: dp[i][j] represents the max product for first i digits with j multiplications
  List<List<int>> dp = List.generate(n + 1, (_) => List.filled(k + 1, 0));
  
  // Base case: no multiplications (j=0), the product is the number formed by first i digits
  for (int i = 1; i <= n; i++) {
    int num = 0;
    for (int l = 0; l < i; l++) {
      num = num * 10 + digits[l];
    }
    dp[i][0] = num;
  }
  
  // Fill the DP table for j from 1 to k
  for (int j = 1; j <= k; j++) {
    for (int i = 1; i <= n; i++) {
      int maxProd = 0;
      // Try all possible splits m where m ranges from j to i-1 (since at least j digits are needed for j multiplications)
      for (int m = j; m < i; m++) {
        int leftPart = dp[m][j - 1];
        int rightPart = 0;
        for (int l = m; l < i; l++) {
          rightPart = rightPart * 10 + digits[l];
        }
        int currentProd = leftPart * rightPart;
        if (currentProd > maxProd) {
          maxProd = currentProd;
        }
      }
      dp[i][j] = maxProd;
    }
  }
  
  return dp[n][k];
}
void testMaxProduct() {
  // Test case 1
  assert(maxProduct("123", 3, 1) == 123, "Test Case 1 Failed: The maximum product from '123' with 1 multiplication sign should be 23 (1 * 23).");

  // Test case 2
  assert(maxProduct("1234", 4, 1) == 1234, "Test Case 2 Failed: The maximum product from '1234' with 1 multiplication sign should be 234 (1 * 234).");

  // Test case 3
  assert(maxProduct("1234", 4, 2) == 1230, "Test Case 3 Failed: The maximum product from '1234' with 2 multiplication signs should be 48 (1 * 2 * 34).");

  print("All maxProduct test cases passed!");
}

void main() {
  testMaxProduct();
}