package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func countTriplets(heights []int, n int) int {
	count := 0
	// Iterate through all possible triplets
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			for k := j + 1; k < n; k++ {
				// Get the three elements
				a, b, c := heights[i], heights[j], heights[k]
				// Find min and max of the triplet
				minVal := a
				if b < minVal {
					minVal = b
				}
				if c < minVal {
					minVal = c
				}
				maxVal := a
				if b > maxVal {
					maxVal = b
				}
				if c > maxVal {
					maxVal = c
				}
				// Check if GCD of min and max is 1
				if gcd(minVal, maxVal) == 1 {
					count++
				}
			}
		}
	}
	return count
}



func TestCountTriplets(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(1, countTriplets([]int{1, 5, 7}, 3))
	assert.Equal(3, countTriplets([]int{1, 6, 2, 3}, 4))
	assert.Equal(0, countTriplets([]int{16, 4, 8, 2}, 4))
	assert.Equal(77, countTriplets([]int{10, 1, 6, 7, 9, 8, 4, 3, 5, 2}, 10))
	assert.Equal(7, countTriplets([]int{4, 5, 9, 11, 14}, 5))
	assert.Equal(104, countTriplets([]int{15, 10, 6, 9, 7, 3, 12, 8, 11, 1, 2}, 11))
	assert.Equal(4, countTriplets([]int{3, 7, 11, 13}, 4))
	assert.Equal(10, countTriplets([]int{5, 12, 13, 17, 19}, 5))
	assert.Equal(87, countTriplets([]int{2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}, 11))
	assert.Equal(122, countTriplets([]int{1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17}, 11))
}