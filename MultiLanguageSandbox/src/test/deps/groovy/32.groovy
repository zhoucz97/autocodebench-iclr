
class ContainerWithMostWater {
    static int maxArea(ArrayList height) {
        """
        Given an integer array 'height' of length n, where the i-th element represents the height of the line at position i.
        Find two lines, which together with the x-axis form a container, such that the container contains the most water.
        Return the maximum amount of water a container can store.
        Note: You may not slant the container.
        >>> maxArea([1,8,6,2,5,4,8,3,7])
        49
        >>> maxArea([1,1])
        1
        """

    int left = 0
    int right = height.size() - 1
    int maxArea = 0
    
    while (left < right) {
        int currentHeight = Math.min(height[left], height[right])
        int currentWidth = right - left
        int currentArea = currentHeight * currentWidth
        maxArea = Math.max(maxArea, currentArea)
        
        if (height[left] < height[right]) {
            left++
        } else {
            right--
        }
    }
    
    return maxArea
}
}
// Test cases
def containerWithMostWater = new ContainerWithMostWater()
assert containerWithMostWater.maxArea([1,8,6,2,5,4,8,3,7]) == 49
assert containerWithMostWater.maxArea([1,1]) == 1
assert containerWithMostWater.maxArea([4,3,2,1,4]) == 16
assert containerWithMostWater.maxArea([1,2,1]) == 2
assert containerWithMostWater.maxArea([1,2,4,3]) == 4
assert containerWithMostWater.maxArea([1,3,2,5,25,24,5]) == 24
assert containerWithMostWater.maxArea([1,8,100,2,100,4,8,3,7]) == 200