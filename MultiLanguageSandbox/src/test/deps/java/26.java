
class Solution {
    /**
     * Given an integer array `gain` representing the net gain in altitude between consecutive points, return the highest altitude reached.
     *
     * Example:
     * >>> highestAltitude([-5,1,5,0,-7])
     * 1
     * Explanation: The altitudes are [0,-5,-4,1,1,-6]. The highest altitude reached is 1.
     *
     * >>> highestAltitude([-4,-3,-2,-1,4,3,2])
     * 0
     * Explanation: The altitudes are [0,-4,-7,-9,-10,-6,-3,-1]. The highest altitude reached is 0.
     *
     * Constraints:
     * - The length of the `gain` array is between 1 and 100, inclusive.
     * - Each element in the `gain` array is between -100 and 100, inclusive.
     */
    public int highestAltitude(int[] gain) 
{
    int currentAltitude = 0;
    int maxAltitude = 0;
    
    for (int g : gain) {
        currentAltitude += g;
        if (currentAltitude > maxAltitude) {
            maxAltitude = currentAltitude;
        }
    }
    
    return maxAltitude;
}
    public static void main(String[] args) {
        Solution solution = new Solution();

        // Test the function with different test cases
        assert solution.highestAltitude(new int[]{-5,1,5,0,-7}) == 1;
        assert solution.highestAltitude(new int[]{-4,-3,-2,-1,4,3,2}) == 0;
        assert solution.highestAltitude(new int[]{-4, -3, -2, -1, 4, 3, 2, 1}) == 0;
        System.out.println("All tests passed");
    }
}