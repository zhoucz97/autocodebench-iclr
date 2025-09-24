
class ThreeSumFinder {
    static List<List<Integer>> threeSum(List<Integer> nums) {
"""
    Given an integer list, finds all unique triplets [nums[i], nums[j], nums[k]] such that
    i != j, i != k, j != k, and nums[i] + nums[j] + nums[k] == 0.
    Returns a list of lists containing the unique triplets.
    
    @param nums List of integers
    @return List of lists containing unique triplets whose sum is zero
    
    Example:
    >>> threeSum([-1, 0, 1, 2, -1, -4])
    Returns: [[-1, -1, 2], [-1, 0, 1]]
    
    >>> threeSum([3, 0, -2, -1, 1, 2])
    Returns: [[-2, -1, 3], [-2, 0, 2], [-1, 0, 1]]
"""

    // Sort the input list to make it easier to avoid duplicates and use two-pointer technique
    nums.sort()
    List<List<Integer>> result = []
    
    for (int i = 0; i < nums.size() - 2; i++) {
        // Skip duplicate elements for the first number
        if (i > 0 && nums[i] == nums[i - 1]) continue
        
        int left = i + 1
        int right = nums.size() - 1
        
        while (left < right) {
            int sum = nums[i] + nums[left] + nums[right]
            
            if (sum == 0) {
                // Found a triplet
                result.add([nums[i], nums[left], nums[right]])
                
                // Skip duplicates for the second number
                while (left < right && nums[left] == nums[left + 1]) left++
                // Skip duplicates for the third number
                while (left < right && nums[right] == nums[right - 1]) right--
                
                left++
                right--
            } else if (sum < 0) {
                // Need a larger number, move left pointer to the right
                left++
            } else {
                // Need a smaller number, move right pointer to the left
                right--
            }
        }
    }
    
    return result
}
}
def threeSumFinder = new ThreeSumFinder()
def result1 = threeSumFinder.threeSum([-1, 0, 1, 2, -1, -4])
assert result1.toSet() == [[-1, -1, 2], [-1, 0, 1]].toSet()

def result2 = threeSumFinder.threeSum([3, 0, -2, -1, 1, 2])
assert result2.toSet() == [[-2, -1, 3], [-2, 0, 2], [-1, 0, 1]].toSet()

def result3 = threeSumFinder.threeSum([1, 2, -2, -1])
assert result3.toSet() == [].toSet()

def result4 = threeSumFinder.threeSum([0, 0, 0])
assert result4.toSet() == [[0, 0, 0]].toSet()

def result5 = threeSumFinder.threeSum([5, -2, 2, -1, 1, 0, 2, 3, -3, 0, 3, 4, -4, 6, 3, 4, -3, -4, 1, -1])
assert result5.toSet() == [[-4, -2, 6], [-4, -1, 5], [-4, 0, 4], [-4, 1, 3], [-4, 2, 2], [-3, -3, 6], [-3, -2, 5], [-3, -1, 4], [-3, 0, 3], [-3, 1, 2], [-2, -1, 3], [-2, 0, 2], [-2, 1, 1], [-1, -1, 2], [-1, 0, 1]].toSet()