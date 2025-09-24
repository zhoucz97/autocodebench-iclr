
class MinSubArrayLen {
    static int minSubArrayLen(int target, int[] nums) {
        """
        Find the minimal length of a contiguous subarray of which the sum is greater than or equal to target.
        If there is no such subarray, return 0 instead.
        >>> minSubArrayLen(7, [2,3,1,2,4,3])
        2
        >>> minSubArrayLen(4, [1,4,4])
        1
        >>> minSubArrayLen(11, [1,1,1,1,1,1,1,1])
        0
        """

    int n = nums.length
    int minLength = Integer.MAX_VALUE
    int left = 0
    int currentSum = 0

    for (int right = 0; right < n; right++) {
        currentSum += nums[right]
        
        while (currentSum >= target) {
            minLength = Math.min(minLength, right - left + 1)
            currentSum -= nums[left]
            left++
        }
    }

    return minLength == Integer.MAX_VALUE ? 0 : minLength
}
}
def minSubArrayLen = new MinSubArrayLen()
assert minSubArrayLen.minSubArrayLen(7, [2,3,1,2,4,3] as int[]) == 2
assert minSubArrayLen.minSubArrayLen(4, [1,4,4] as int[]) == 1
assert minSubArrayLen.minSubArrayLen(11, [1,1,1,1,1,1,1,1] as int[]) == 0
assert minSubArrayLen.minSubArrayLen(15, [1,2,3,4,5] as int[]) == 5
assert minSubArrayLen.minSubArrayLen(100, [1,2,3,4,5,6,7,8,9,10] as int[]) == 0
assert minSubArrayLen.minSubArrayLen(15, [5,1,3,5,10,7,4,9,2,8] as int[]) == 2
assert minSubArrayLen.minSubArrayLen(11, [1,2,3,4,5,6,7,8,9,10] as int[]) == 2