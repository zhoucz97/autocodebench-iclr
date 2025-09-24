package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func makeEqualAgain(n int, a []int) int {
    if n == 0 {
        return 0
    }
    
    // Check if all elements are the same
    allSame := true
    first := a[0]
    for _, num := range a {
        if num != first {
            allSame = false
            break
        }
    }
    if allSame {
        return 0
    }
    
    minCost := n // worst case: replace the entire array
    
    // Check left part: find the first position where a[i] != first
    left := 0
    for left < n && a[left] == first {
        left++
    }
    if left < n {
        // The cost is the length of the right part (n - left)
        cost := n - left
        if cost < minCost {
            minCost = cost
        }
    }
    
    // Check right part: find the last position where a[i] != first
    right := n - 1
    for right >= 0 && a[right] == first {
        right--
    }
    if right >= 0 {
        // The cost is the length of the left part (right + 1)
        cost := right + 1
        if cost < minCost {
            minCost = cost
        }
    }
    
    // Check middle parts: find segments where a[i] == first, then the cost is the length of the segment between them
    // We need to find the minimal segment between two positions where a[i] == first that covers the entire array when replaced
    // But another approach is to find the minimal segment that, when replaced, makes the entire array first
    // So, the minimal segment is the one between the first occurrence of a value different from first and the last occurrence of a value different from first
    // But wait, no. The correct approach is to find the minimal segment that, when replaced, makes all elements equal to first.
    // So, the segment to replace is the one that starts after the last position where a[i] == first in the left and ends before the first position where a[i] == first in the right.
    // But perhaps the minimal cost is the minimal between left and right approaches, or another segment.
    // However, the examples suggest that the answer is the minimal between the left and right approaches.
    // For example, in [1,2,1], left is 1 (cost 2), right is 1 (cost 1). The minimal is 1.
    // In [5,5,1,5,5], left is 2 (cost 3), right is 2 (cost 3). The minimal is 1 (replace the middle 1).
    // So, the initial approach may not cover all cases.
    // Hence, we need to find the minimal segment that, when replaced, makes all elements equal to the majority value.
    // But the problem allows replacing any segment to any value, not necessarily the majority.
    // So, the correct approach is to find the minimal segment that, when replaced, makes all elements equal to some value.
    // The optimal value to replace to is the value that appears most frequently in the array, but not necessarily.
    // However, the examples show that sometimes replacing a single element suffices.
    // So, the minimal cost is the minimal between:
    // - the cost to make all elements equal to the first element (left or right approach)
    // - the cost to make all elements equal to the last element (similar left/right)
    // - other possibilities.
    // But given the constraints, perhaps the minimal cost is the minimal between the left and right approaches.
    // Let's test the second example:
    // [5,5,1,5,5]: left is 2 (positions 0,1 are 5; cost 3 (positions 2,3,4)), right is 2 (positions 3,4 are 5; cost 3 (positions 0,1,2)). But the answer is 1 (replace position 2).
    // So, the initial approach misses this case.
    // Hence, we need a different approach.
    // The correct approach is to find the minimal segment that, when replaced, makes all elements equal to some value x.
    // The value x can be any value in the array, but the optimal x is likely one that appears most frequently.
    // However, the problem allows replacing to any x, not necessarily present in the array.
    // So, the minimal cost is the minimal length of a segment such that all elements outside this segment are the same, and then the segment can be set to that value.
    // So, the algorithm is:
    // 1. Find the leftmost and rightmost positions where the array elements differ from the first element.
    // 2. The cost is the minimal between (rightmost - leftmost + 1) and other possible segments.
    // But in the second example, leftmost is 2 (value 1), rightmost is 2. So the segment length is 1, which matches the answer.
    // So, the correct approach is:
    // Find the leftmost position where a[i] != first, and the rightmost position where a[i] != first.
    // The minimal cost is the minimal between (right - left + 1), left, and (n - right - 1).
    // But in the first example, left is 1, right is 1. So right - left + 1 is 1, which matches the answer.
    // So, the algorithm is:
    leftDiff := -1
    rightDiff := -1
    for i := 0; i < n; i++ {
        if a[i] != a[0] {
            leftDiff = i
            break
        }
    }
    for i := n - 1; i >= 0; i-- {
        if a[i] != a[0] {
            rightDiff = i
            break
        }
    }
    if leftDiff == -1 {
        return 0 // all elements are the same
    }
    // The minimal cost is the minimal between:
    // 1. The cost to make all elements equal to a[0]: replace the segment from leftDiff to rightDiff (cost rightDiff - leftDiff + 1)
    // 2. The cost to make all elements equal to a[rightDiff+1 ...]: replace the left part (cost leftDiff + 1)
    // 3. The cost to make all elements equal to a[0 ... leftDiff-1]: replace the right part (cost n - rightDiff - 1)
    cost1 := rightDiff - leftDiff + 1
    cost2 := leftDiff + 1
    cost3 := n - rightDiff - 1
    minCost = min(minCost, cost1, cost2, cost3)
    
    return minCost
}

func min(a ...int) int {
    m := a[0]
    for _, v := range a[1:] {
        if v < m {
            m = v
        }
    }
    return m
}

func TestMakeEqualAgain(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(1, makeEqualAgain(3, []int{1, 2, 1}))
	assert.Equal(1, makeEqualAgain(5, []int{5, 5, 1, 5, 5}))
	assert.Equal(0, makeEqualAgain(4, []int{1, 1, 1, 1}))
	assert.Equal(1, makeEqualAgain(6, []int{2, 2, 2, 3, 2, 2}))
	assert.Equal(0, makeEqualAgain(1, []int{1}))
	assert.Equal(1, makeEqualAgain(2, []int{1, 2}))
	assert.Equal(2, makeEqualAgain(4, []int{1, 2, 2, 1}))
	assert.Equal(2, makeEqualAgain(7, []int{4, 4, 4, 3, 3, 4, 4}))
	assert.Equal(3, makeEqualAgain(6, []int{5, 4, 4, 4, 5, 5}))
	assert.Equal(5, makeEqualAgain(7, []int{1, 2, 1, 2, 1, 2, 1}))
	assert.Equal(4, makeEqualAgain(6, []int{1, 2, 3, 4, 5, 1}))
	assert.Equal(0, makeEqualAgain(7, []int{1, 1, 1, 1, 1, 1, 1}))
	assert.Equal(2, makeEqualAgain(8, []int{8, 8, 8, 1, 2, 8, 8, 8}))
	assert.Equal(2, makeEqualAgain(3, []int{1, 2, 3}))
	assert.Equal(6, makeEqualAgain(7, []int{4, 3, 2, 7, 1, 1, 3}))
	assert.Equal(7, makeEqualAgain(9, []int{9, 9, 2, 9, 2, 5, 5, 5, 3}))
}