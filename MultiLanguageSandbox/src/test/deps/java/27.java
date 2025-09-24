
class Solution {
    /**
     * Given an integer array nums, return the maximum absolute sum of any subarray.
     *
     * >>> maxAbsoluteSum(new int[]{1,-3,2,3,-4})
     * 5
     *
     * >>> maxAbsoluteSum(new int[]{2,-5,1,-4,3,-2})
     * 8
     */
    public int maxAbsoluteSum(int[] nums) 
{
    if (nums == null || nums.length == 0) {
        return 0;
    }
    
    int maxEndingHere = nums[0];
    int minEndingHere = nums[0];
    int maxSoFar = nums[0];
    int minSoFar = nums[0];
    
    for (int i = 1; i < nums.length; i++) {
        maxEndingHere = Math.max(nums[i], maxEndingHere + nums[i]);
        maxSoFar = Math.max(maxSoFar, maxEndingHere);
        
        minEndingHere = Math.min(nums[i], minEndingHere + nums[i]);
        minSoFar = Math.min(minSoFar, minEndingHere);
    }
    
    return Math.max(Math.abs(maxSoFar), Math.abs(minSoFar));
}
public static void main(String[] args) {
        Solution solution = new Solution();

        // Test the function with different test cases
        assert solution.maxAbsoluteSum(new int[]{1,-3,2,3,-4}) == 5;
        assert solution.maxAbsoluteSum(new int[]{2,-5,1,-4,3,-2}) == 8;
        assert solution.maxAbsoluteSum(new int[]{-2, -3, 4, -1, -2, 1, 5, -3}) == 7;
        System.out.println("All tests passed");
    }
}