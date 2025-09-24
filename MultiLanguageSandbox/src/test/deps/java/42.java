import java.util.*;
import java.util.HashSet;
import java.util.Set;

public class Solution {

  /**
   * Given a non-negative integer array A, if the sum of each pair of adjacent elements in the array is a perfect square, then the array is considered a square array.
   * Return the number of square permutations of A. Two permutations A1 and A2 are considered different if there exists an index i such that A1[i] != A2[i].
   *
   * @param A The input non-negative integer array.
   * @return The number of square permutations of A.
   *
   * Example:
   * squarePermutations(new int[]{1, 17, 8})
   * Output: 2
   *
   * Example:
   * squarePermutations(new int[]{2, 2, 2})
   * Output: 1
   */
  public static int squarePermutations(int[] A) 
{
        List<Integer> list = new ArrayList<>();
        for (int num : A) {
            list.add(num);
        }
        boolean[] used = new boolean[A.length];
        return backtrack(list, used, 0, -1);
    }

    private static int backtrack(List<Integer> nums, boolean[] used, int pos, int prev) {
        if (pos == nums.size()) {
            return 1;
        }
        int count = 0;
        for (int i = 0; i < nums.size(); i++) {
            if (!used[i]) {
                if (pos == 0 || isPerfectSquare(prev + nums.get(i))) {
                    used[i] = true;
                    count += backtrack(nums, used, pos + 1, nums.get(i));
                    used[i] = false;
                }
            }
        }
        return count;
    }

    private static boolean isPerfectSquare(int num) {
        if (num < 0) {
            return false;
        }
        int sqrt = (int) Math.sqrt(num);
        return sqrt * sqrt == num;
    }
  public static void main(String[] args) {
    assert squarePermutations(new int[] { 1, 8, 27 }) == 0;
    assert squarePermutations(new int[] { 1, 8, 17 }) == 2;
    assert squarePermutations(new int[] { 4, 1, 3, 0, 2 }) == 0;
    System.out.println("All tests passed");
  }
}