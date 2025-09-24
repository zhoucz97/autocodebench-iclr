
class JumpGameChecker {
    static boolean canJumpToEnd(List nums) {
        """
        Given a non-negative integer array 'nums', where each element represents the maximum
        length of a jump at that position. Determine if you are able to reach the last index.
        You can assume that you can always reach the first index.
        >>> canJumpToEnd([2,3,1,1,4])
        True
        >>> canJumpToEnd([3,2,1,0,4])
        False
        """

    int maxReach = 0
    for (int i = 0; i < nums.size(); i++) {
        // If current position is beyond the maximum reachable index
        if (i > maxReach) {
            return false
        }
        // Update the maximum reachable index
        maxReach = Math.max(maxReach, i + nums[i])
        // If we've reached or passed the last index, return true
        if (maxReach >= nums.size() - 1) {
            return true
        }
    }
    return true
}
}
// Test cases
def jumpGameChecker = new JumpGameChecker()
assert jumpGameChecker.canJumpToEnd([2,3,1,1,4]) == true
assert jumpGameChecker.canJumpToEnd([3,2,1,0,4]) == false
assert jumpGameChecker.canJumpToEnd([1,1,1,1,1]) == true
assert jumpGameChecker.canJumpToEnd([0,0,0,0,0]) == false
assert jumpGameChecker.canJumpToEnd([1,2,3,4,5]) == true
assert jumpGameChecker.canJumpToEnd([5,4,3,2,1]) == true
assert jumpGameChecker.canJumpToEnd([1,0,1,0,1]) == false