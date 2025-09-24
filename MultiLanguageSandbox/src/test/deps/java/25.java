
class Solution {
  /**
  * Given an integer n, return the minimum number of operations required to make all elements in the array arr equal.
  * The array arr has a length of n, and arr[i] = (2 * i) + 1 for 0 <= i < n.
  * In one operation, you can select two indices x and y (0 <= x, y < n) and subtract 1 from arr[x] and add 1 to arr[y] (i.e., arr[x] -= 1 and arr[y] += 1).
  * The goal is to make all elements in arr equal. The test cases will ensure that after performing some operations, all elements in arr can be made equal.
  *
  * Example 1:
  * Input: n = 3
  * Output: 2
  * Explanation: arr = [1, 3, 5]
  * First operation: x = 2, y = 0 -> arr = [2, 3, 4]
  * Second operation: x = 2, y = 0 -> arr = [3, 3, 3]
  *
  * Example 2:
  * Input: n = 6
  * Output: 9
  */
  public int minOperations(int n) 
{
    int operations = 0;
    int target = (2 * (n / 2)) + 1; // For odd n, this is the middle element; for even, it's the higher middle
    if (n % 2 == 0) {
        target = (2 * (n / 2 - 1)) + 1 + 1; // Adjust for even n: average of two middle elements is (middle1 + middle2)/2, but since operations must result in integers, the target is the higher middle for even n (e.g., for n=6, middle elements are 5 and 7, target is 6)
        // Wait, no. For n=6, the array is [1,3,5,7,9,11]. The target is (5+7)/2 =6. But operations would be sum of (6-1)+(6-3)+(6-5) =5+3+1=9. So the target is indeed 6.
        // So for even n, the target is (2*(n/2 -1) +1 + 2*(n/2) +1)/2 = ( (2k-1) + (2k+1) )/2 = (4k)/2 = 2k, where k is n/2. So for n=6, k=3, 2*3=6.
        target = 2 * (n / 2);
    } else {
        target = (2 * (n / 2)) + 1;
    }
    
    for (int i = 0; i < n / 2; i++) {
        int current = (2 * i) + 1;
        operations += (target - current);
    }
    return operations;
}
  public static void main(String[] args) {
      Solution solution = new Solution();

      // Test the function with different test cases
      assert solution.minOperations(4) == 4;
      assert solution.minOperations(5) == 6;
      assert solution.minOperations(7) == 12;
      System.out.println("All tests passed");
  }
}