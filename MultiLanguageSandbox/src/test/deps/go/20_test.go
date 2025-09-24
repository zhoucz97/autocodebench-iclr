package main
import (
    "math"
    "testing"
    "github.com/stretchr/testify/assert"
)

func IsSquare(length int, squares []int) bool {
	total := 0
	for _, num := range squares {
		total += num
	}
	side := int(math.Sqrt(float64(total)))
	return side*side == total
}

func TestIsSquare(t *testing.T) {
	assert := assert.New(t)
	assert.True(IsSquare(1, []int{9}))
	assert.True(IsSquare(2, []int{14, 2}))
	assert.False(IsSquare(2, []int{7, 7}))
	assert.False(IsSquare(7, []int{1, 2, 3, 4, 5, 6, 7}))
	assert.True(IsSquare(6, []int{1, 3, 5, 7, 9, 11}))
	assert.False(IsSquare(4, []int{2, 2, 2, 2}))

	// Additional test cases
	assert.False(IsSquare(3, []int{4, 5, 6}))
	assert.False(IsSquare(4, []int{16, 9, 4, 1}))
	assert.False(IsSquare(5, []int{1, 1, 1, 1, 1}))
	assert.False(IsSquare(2, []int{25, 25}))
	assert.True(IsSquare(3, []int{10, 10, 5}))
}