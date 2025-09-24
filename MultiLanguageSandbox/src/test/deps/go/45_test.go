package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func minOperations(pixels []int) int {
    if len(pixels) == 0 {
        return 0
    }

    // Collect all unique colors in the pixels
    colorSet := make(map[int]bool)
    for _, color := range pixels {
        colorSet[color] = true
    }

    minOps := len(pixels) // Initialize with a large number

    for targetColor := range colorSet {
        operations := 0
        i := 0
        n := len(pixels)
        for i < n {
            if pixels[i] == targetColor {
                // Skip all connected pixels of targetColor
                for i < n && pixels[i] == targetColor {
                    i++
                }
            } else {
                // Need an operation to change this connected component to targetColor
                operations++
                // Skip all connected pixels of the current color
                currentColor := pixels[i]
                for i < n && pixels[i] == currentColor {
                    i++
                }
            }
        }
        if operations < minOps {
            minOps = operations
        }
    }

    return minOps
}

func TestMinOperations(t *testing.T) {
	assert := assert.New(t)

	assert.Equal(2, minOperations([]int{1, 2, 3, 2, 1}))
	assert.Equal(1, minOperations([]int{1, 1, 2, 2}))
	assert.Equal(3, minOperations([]int{1, 2, 1, 4, 2}))
	assert.Equal(0, minOperations([]int{5, 5, 5, 5, 5}))
	assert.Equal(1, minOperations([]int{1, 1, 1, 2, 2, 2}))
	assert.Equal(2, minOperations([]int{1, 3, 3, 3, 2, 2, 2}))
	assert.Equal(1, minOperations([]int{4, 4, 4, 4, 3, 3, 3, 3}))
	assert.Equal(8, minOperations([]int{1, 2, 3, 4, 5, 6, 7, 8, 9}))
	assert.Equal(5, minOperations([]int{1, 2, 1, 2, 1, 2, 1, 2, 1, 2}))
	assert.Equal(0, minOperations([]int{3, 3, 3}))
	assert.Equal(1, minOperations([]int{2, 1, 1, 2}))
}