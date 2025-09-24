import java.util.ArrayList;
import java.util.List;
import java.lang.*;
import java.util.*;

class Solution {

  /**
   * Given an array of distinct integers, return all possible permutations of the array.
   * >>> permute(new int[]{1,2,3})
   * [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
   * >>> permute(new int[]{0,1})
   * [[0,1],[1,0]]
   * >>> permute(new int[]{1})
   * [[1]]
   */
  public List<List<Integer>> permute(int[] nums) 
{
        List<List<Integer>> result = new ArrayList<>();
        backtrack(nums, 0, result);
        return result;
    }
    
    private void backtrack(int[] nums, int start, List<List<Integer>> result) {
        if (start == nums.length - 1) {
            // Convert the current permutation to a list and add to result
            List<Integer> permutation = new ArrayList<>();
            for (int num : nums) {
                permutation.add(num);
            }
            result.add(permutation);
            return;
        }
        
        for (int i = start; i < nums.length; i++) {
            // Swap the current element with the start element
            swap(nums, start, i);
            // Recursively generate permutations for the remaining elements
            backtrack(nums, start + 1, result);
            // Backtrack (swap back to original position)
            swap(nums, start, i);
        }
    }
    
    private void swap(int[] nums, int i, int j) {
        int temp = nums[i];
        nums[i] = nums[j];
        nums[j] = temp;
    }
public static void main(String[] args) {
    Solution solution = new Solution();

    // Test the function with different test cases
    assert solution
      .permute(new int[] { 4, 5, 6 })
      .equals(
        Arrays.asList(
          Arrays.asList(4, 5, 6),
          Arrays.asList(4, 6, 5),
          Arrays.asList(5, 4, 6),
          Arrays.asList(5, 6, 4),
          Arrays.asList(6, 4, 5),
          Arrays.asList(6, 5, 4)
        )
      );
    assert solution
      .permute(new int[] { 7, 8 })
      .equals(Arrays.asList(Arrays.asList(7, 8), Arrays.asList(8, 7)));
    assert solution
      .permute(new int[] { 9 })
      .equals(Collections.singletonList(Collections.singletonList(9)));
    System.out.println("All tests passed");
  }
}