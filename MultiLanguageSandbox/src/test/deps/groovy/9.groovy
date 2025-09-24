
class PeakElementFinder {
    static int findPeakElement(List<Integer> nums) {
        """
        Find a peak element in the given array. A peak element is an element that is greater than its neighbors.
        The array may contain multiple peaks, in which case, return the index of any one of them.
        You may assume that nums[-1] = nums[n] = -∞.
        You must solve the problem in O(log n) time complexity.
        >>> findPeakElement([1,2,3,1])
        2
        >>> findPeakElement([1,2,1,3,5,6,4])
        1 or 5
        """

    int left = 0
    int right = nums.size() - 1
    
    while (left < right) {
        int mid = left + (right - left) / 2
        if (nums[mid] < nums[mid + 1]) {
            left = mid + 1
        } else {
            right = mid
        }
    }
    return left
}
}
def peakElementFinder = new PeakElementFinder()
assert peakElementFinder.findPeakElement([1, 2, 3, 1]) == 2
assert peakElementFinder.findPeakElement([1, 2, 1, 3, 5, 6, 4]) == 1 || peakElementFinder.findPeakElement([1, 2, 1, 3, 5, 6, 4]) == 5
assert peakElementFinder.findPeakElement([1, 2, 3, 4, 5, 6, 7]) == 6
assert peakElementFinder.findPeakElement([7, 6, 5, 4, 3, 2, 1]) == 0
assert peakElementFinder.findPeakElement([1, 1, 1, 1, 1, 1, 1]) == 0 || 
       peakElementFinder.findPeakElement([1, 1, 1, 1, 1, 1, 1]) == 1 || 
       peakElementFinder.findPeakElement([1, 1, 1, 1, 1, 1, 1]) == 2 || 
       peakElementFinder.findPeakElement([1, 1, 1, 1, 1, 1, 1]) == 3 || 
       peakElementFinder.findPeakElement([1, 1, 1, 1, 1, 1, 1]) == 4 || 
       peakElementFinder.findPeakElement([1, 1, 1, 1, 1, 1, 1]) == 5 || 
       peakElementFinder.findPeakElement([1, 1, 1, 1, 1, 1, 1]) == 6