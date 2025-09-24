import java.util.LinkedList
import java.util.Queue
class MaxSlidingWindow {
    static List<Integer> maxSlidingWindow(List<Integer> nums, int k) {  
        """
        Given an integer array 'nums' and an integer 'k', return an array where the i-th element is the maximum number in the window of size 'k' starting from position i.
        >>> maxSlidingWindow([1, 3, -1, -3, 5, 3, 6, 7], 3)
        [3, 3, 5, 5, 6, 7]
        >>> maxSlidingWindow([1], 1)
        [1]
        """

    if (nums == null || nums.isEmpty() || k <= 0) {
        return []
    }
    
    Queue<Integer> deque = new LinkedList<>()
    List<Integer> result = []
    
    for (int i = 0; i < nums.size(); i++) {
        // Remove elements from the deque that are out of the current window
        while (!deque.isEmpty() && deque.peek() < i - k + 1) {
            deque.poll()
        }
        
        // Remove elements from the deque that are smaller than the current element
        while (!deque.isEmpty() && nums[deque.peekLast()] < nums[i]) {
            deque.pollLast()
        }
        
        // Add current element to the deque
        deque.offer(i)
        
        // The front of the deque is the maximum for the current window
        if (i >= k - 1) {
            result.add(nums[deque.peek()])
        }
    }
    
    return result
}
}
def maxSlidingWindow = new MaxSlidingWindow()
assert maxSlidingWindow.maxSlidingWindow([1, 3, -1, -3, 5, 3, 6, 7], 3) == [3, 3, 5, 5, 6, 7]
assert maxSlidingWindow.maxSlidingWindow([1], 1) == [1]
assert maxSlidingWindow.maxSlidingWindow([1, 2, 3, 4, 5, 6, 7], 3) == [3, 4, 5, 6, 7]
assert maxSlidingWindow.maxSlidingWindow([7, 6, 5, 4, 3, 2, 1], 3) == [7, 6, 5, 4, 3]
assert maxSlidingWindow.maxSlidingWindow([1, 1, 1, 1, 1, 1, 1], 3) == [1, 1, 1, 1, 1]
assert maxSlidingWindow.maxSlidingWindow([1, 2, 3, 2, 1, 2, 3], 3) == [3, 3, 3, 2, 3]
assert maxSlidingWindow.maxSlidingWindow([1, 2, 3, 4, 5, 6, 7], 1) == [1, 2, 3, 4, 5, 6, 7]