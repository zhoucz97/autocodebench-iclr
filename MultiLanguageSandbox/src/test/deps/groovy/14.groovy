
class DuplicateFinder {
    static int findDuplicate(List<Integer> nums) {
        """
        Given an array of integers 'nums' containing 'n + 1' integers where each integer is in the range [1, n] inclusive.
        There is only one repeated number in 'nums', return this repeated number.
        The solution must not modify the array 'nums' and only use constant extra space.

        >>> findDuplicate([1, 3, 4, 2, 2])
        2
        >>> findDuplicate([3, 1, 3, 4, 2])
        3
        """

    int slow = nums[0]
    int fast = nums[0]
    
    // Phase 1: Find the intersection point of the two runners.
    do {
        slow = nums[slow]
        fast = nums[nums[fast]]
    } while (slow != fast)
    
    // Phase 2: Find the "entrance" to the cycle.
    slow = nums[0]
    while (slow != fast) {
        slow = nums[slow]
        fast = nums[fast]
    }
    
    return slow
}
}
def duplicateFinder = new DuplicateFinder()
assert duplicateFinder.findDuplicate([1, 3, 4, 2, 2]) == 2
assert duplicateFinder.findDuplicate([3, 1, 3, 4, 2]) == 3
assert duplicateFinder.findDuplicate([1, 2, 3, 4, 5, 5]) == 5
assert duplicateFinder.findDuplicate([5, 1, 2, 3, 4, 5]) == 5
assert duplicateFinder.findDuplicate([1, 1]) == 1
assert duplicateFinder.findDuplicate([2, 5, 9, 6, 9, 3, 8, 9, 7, 1]) == 9