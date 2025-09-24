package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func FindMaxMin(a, b, c int) (int, int) {
    // Initialize max and min with the first value
    max := a
    min := a

    // Compare with the second value
    if b > max {
        max = b
    }
    if b < min {
        min = b
    }

    // Compare with the third value
    if c > max {
        max = c
    }
    if c < min {
        min = c
    }

    return max, min
}



func TestFindMaxMin(t *testing.T) {
	assert := assert.New(t)
	var max, min int

	max, min = FindMaxMin(1, 2, 3)
	assert.Equal(3, max)
	assert.Equal(1, min)

	// Additional tests
	max, min = FindMaxMin(5, 3, 4)
	assert.Equal(5, max)
	assert.Equal(3, min)

	max, min = FindMaxMin(10, -2, 7)
	assert.Equal(10, max)
	assert.Equal(-2, min)

	max, min = FindMaxMin(-1, -3, -2)
	assert.Equal(-1, max)
	assert.Equal(-3, min)
}