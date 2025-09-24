
import 'dart:math';

int getMaxValue(int totalMoney, int numItems, List<int> prices, List<int> importance)
/*Calculates the maximum value one can obtain with a given amount of money and a list of items with associated prices and importance.

This function uses a dynamic programming approach to determine the maximum value that can be achieved
within the constraints of the total money available to spend. Each item's value is determined by its
price multiplied by its importance. The function iterates over the list of items, updating the maximum
value obtainable at each price point up to the total money available.

Arguments:
  totalMoney (int): The total amount of money available to spend on items.
  numItems (int): The number of different items available for purchase.
  prices (List<int>): A list containing the price of each item.
  importance (List<int>): A list containing the importance of each item, corresponding to the prices list.

Returns:
  int: The maximum value obtainable with the given total money and items.

Examples:
    getMaxValue(50, 2, [20, 30], [5, 10])
    returns 400
*/
{
  // Create a DP array where dp[i] represents the maximum value achievable with i money
  List<int> dp = List.filled(totalMoney + 1, 0);

  for (int i = 0; i < numItems; i++) {
    int price = prices[i];
    int value = price * importance[i];
    
    // Iterate from the end to avoid reusing the same item multiple times
    for (int j = totalMoney; j >= price; j--) {
      if (dp[j - price] + value > dp[j]) {
        dp[j] = dp[j - price] + value;
      }
    }
  }

  return dp[totalMoney];
}
void testGetMaxValue() {
  // Test case 1: Basic scenario with two items
  assert(
    getMaxValue(50, 2, [20, 30], [5, 10]) == 400,
    'Test Case 1 Failed: Expected maximum value to be 60, got ${getMaxValue(50, 2, [20, 30], [5, 10])}'
  );

  // Test case 2: Scenario where the total money can't buy the most valuable item
  assert(
    getMaxValue(10, 3, [5, 10, 15], [3, 6, 9]) == 60,
    'Test Case 2 Failed: Expected maximum value to be 6, got ${getMaxValue(10, 3, [5, 10, 15], [3, 6, 9])}'
  );

  // Test case 3: Multiple items can be bought within the total money
  assert(
    getMaxValue(100, 4, [50, 25, 10, 20], [1, 2, 3, 2]) == 140,
    'Test Case 3 Failed: Expected maximum value to be 100, got ${getMaxValue(100, 4, [50, 25, 10, 20], [1, 2, 3, 2])}'
  );

  // Additional test cases can be added here as needed.

  print('All test cases passed!');
}

void main() {
  testGetMaxValue();
}