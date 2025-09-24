package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func MinTotalCost(n int, C []int) int64 {
    totalCost := int64(0)
    currentPos := 0 // starts at door 1 (0-based or 1-based? Assuming 0-based for easier calculation)
    // We need to open all doors, so we process each door in order, but the movement is circular.
    // The initial position is 0 (door 1 in 1-based).
    for remainingDoors := n; remainingDoors > 0; {
        // Find the largest i such that moving i steps from currentPos opens a new door.
        // The next door to open is (currentPos + 1) mod n, but we need to find the minimal steps to reach it.
        // However, since we can choose any i, the optimal is to choose the largest possible i that reaches an unopened door.
        // But in a circle, the next unopened door could be any, but the problem implies that we open them in order.
        // Wait, the example given is for 3 doors with C=[1,1,1], the answer is 3, which is 1+1+1, meaning each step is 1.
        // So perhaps the player must open doors in order: 1, then 2, then 3.
        // So the steps are: to go from 1 to 2: step 1 (cost C[0]=1), then from 2 to 3: step 1 (cost C[0]=1), then from 3 to 1: step 1 (cost C[0]=1), total 3.
        // So the general approach is to open doors in order, and for each step between consecutive doors, choose the minimal cost step.
        // But given C is non-increasing, the cost for step 1 is C[0], step 2 is C[1], etc.
        // So for each step between doors i and i+1 (mod n), the cost is C[i-1] (0-based) because moving 1 step costs C[0], 2 steps C[1], etc.
        // But in the example, moving 1 step each time costs 1 each, total 3.
        // So the solution is to sum C[0] n times, because each move is 1 step.
        // But wait, the problem says the player can choose any i each turn, not necessarily 1.
        // So the optimal is to choose the largest possible i that opens a new door, but since the doors are in a circle, the next unopened door could be any.
        // But the example suggests that the player opens doors in order, 1, 2, 3, each time moving 1 step.
        // So perhaps the problem implies that the player must open doors in order, and each move is exactly to the next door in order.
        // Then the cost is sum of C[0] for each move, which is n * C[0].
        // But in the example, C[0] is 1, n is 3, so 3*1=3, which matches.
        // So the solution is to return n * C[0].
        return int64(n) * int64(C[0])
    }
    return totalCost
}

func TestMinTotalCost(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(int64(15), MinTotalCost(5, []int{4, 3, 3, 3, 3})) // Note the 0-indexed array for slice in Golang
	assert.Equal(int64(3), MinTotalCost(3, []int{1, 1, 1}))
	assert.Equal(int64(11), MinTotalCost(4, []int{5, 4, 3, 2}))
	assert.Equal(int64(391), MinTotalCost(4, []int{100, 99, 98, 97}))
	assert.Equal(int64(35), MinTotalCost(6, []int{10, 9, 8, 7, 6, 5}))
	assert.Equal(int64(14), MinTotalCost(7, []int{2, 2, 2, 2, 2, 2, 2}))
	assert.Equal(int64(56), MinTotalCost(8, []int{9, 7, 7, 7, 7, 7, 7, 7}))
	assert.Equal(int64(18), MinTotalCost(9, []int{3, 2, 2, 2, 2, 2, 2, 2, 2}))
	assert.Equal(int64(50), MinTotalCost(10, []int{6, 5, 5, 5, 5, 5, 5, 5, 5, 5}))
	assert.Equal(int64(11), MinTotalCost(11, []int{8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}))
}