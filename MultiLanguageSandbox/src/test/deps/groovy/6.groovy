
class RotatedArraySearch {
    static int search(List<Integer> nums, int target) {
        """
        Search for a target in a rotated sorted array.
        The array is rotated at some pivot unknown to you beforehand.
        (i.e., [0,1,2,4,5,6,7] might become [4,5,6,7,0,1,2]).
        You are given a target value to search. If found in the array return its index, otherwise return -1.
        You may assume no duplicate exists in the array.
        Your algorithm's runtime complexity must be in the order of O(log n).
        >>> search([4,5,6,7,0,1,2], 0)
        4
        >>> search([4,5,6,7,0,1,2], 3)
        -1
        >>> search([1], 0)
        -1
        """

    int low = 0
    int high = nums.size() - 1
    
    while (low <= high) {
        int mid = low + (high - low) / 2
        if (nums[mid] == target) {
            return mid
        }
        
        // Check if the left half is sorted
        if (nums[low] <= nums[mid]) {
            // Target is in the left half
            if (target >= nums[low] && target < nums[mid]) {
                high = mid - 1
            } else {
                low = mid + 1
            }
        } else {
            // Right half is sorted
            if (target > nums[mid] && target <= nums[high]) {
                low = mid + 1
            } else {
                high = mid - 1
            }
        }
    }
    
    return -1
}
}
def rotatedArraySearch = new RotatedArraySearch()
assert rotatedArraySearch.search([4,5,6,7,0,1,2], 0) == 4
assert rotatedArraySearch.search([4,5,6,7,0,1,2], 3) == -1
assert rotatedArraySearch.search([1], 0) == -1
assert rotatedArraySearch.search([4,5,6,7,0,1,2], 5) == 1
assert rotatedArraySearch.search([4,5,6,7,0,1,2], 1) == 5
assert rotatedArraySearch.search([4,5,6,7,0,1,2], 7) == 3
assert rotatedArraySearch.search([4,5,6,7,0,1,2], 4) == 0
assert rotatedArraySearch.search([4,5,6,7,0,1,2], 2) == 6