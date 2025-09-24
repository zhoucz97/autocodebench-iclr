package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func MaxModeSum(n int, counts []int) int64 {
    total := 0
    for _, cnt := range counts {
        total += cnt
    }
    sum := int64(0)
    // We process numbers from n down to 1
    for i := n; i >= 1; i-- {
        cnt := counts[i-1]
        if cnt == 0 {
            continue
        }
        // The current number i will be the mode for 'cnt' new positions added
        // Each time we add i, it becomes the mode for the next 'cnt' steps
        sum += int64(i * cnt)
    }
    return sum
}

func TestMaxModeSum(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(int64(17), MaxModeSum(3, []int{1, 3, 2}))
	assert.Equal(int64(37), MaxModeSum(4, []int{4, 1, 2, 3}))
	assert.Equal(int64(4), MaxModeSum(2, []int{1, 1}))
	assert.Equal(int64(75), MaxModeSum(5, []int{1, 2, 3, 4, 5}))
	assert.Equal(int64(100000), MaxModeSum(1, []int{100000}))
	assert.Equal(int64(62), MaxModeSum(5, []int{5, 3, 2, 4, 1}))
	assert.Equal(int64(900000), MaxModeSum(3, []int{100000, 100000, 100000}))
	assert.Equal(int64(27), MaxModeSum(3, []int{2, 2, 5}))
	assert.Equal(int64(64), MaxModeSum(4, []int{4, 4, 4, 4}))
	assert.Equal(int64(126), MaxModeSum(6, []int{1, 2, 3, 4, 5, 6}))
	assert.Equal(int64(16), MaxModeSum(3, []int{3, 1, 2}))
}