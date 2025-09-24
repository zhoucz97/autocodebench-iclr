package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func CountPermutations(n int, k int, q []int) int {
	// Check if q can be split into k increasing segments
	segmentCount := 1
	for i := 1; i < n; i++ {
		if q[i] < q[i-1] {
			segmentCount++
		}
	}
	if segmentCount != k {
		return 0
	}

	// Now, compute the factorial of each segment's length and multiply them
	factorials := make([]int, n+1)
	factorials[0] = 1
	for i := 1; i <= n; i++ {
		factorials[i] = (factorials[i-1] * i) % mod
	}

	// Determine the lengths of each segment
	segmentLengths := make([]int, 0)
	currentLength := 1
	for i := 1; i < n; i++ {
		if q[i] < q[i-1] {
			segmentLengths = append(segmentLengths, currentLength)
			currentLength = 1
		} else {
			currentLength++
		}
	}
	segmentLengths = append(segmentLengths, currentLength)

	// Calculate the product of the factorials of the segment lengths
	result := 1
	for _, len := range segmentLengths {
		result = (result * factorials[len]) % mod
	}

	return result
}



func TestCountPermutations(t *testing.T) {
	assert := assert.New(t)

	assert.Equal(2, CountPermutations(2, 1, []int{1, 2}))
	assert.Equal(1, CountPermutations(3, 3, []int{3, 1, 2}))
	assert.Equal(13, CountPermutations(6, 3, []int{1, 2, 3, 6, 5, 4}))
	assert.Equal(720, CountPermutations(6, 1, []int{1, 2, 3, 4, 5, 6}))
	assert.Equal(0, CountPermutations(6, 3, []int{1, 2, 5, 3, 4, 5}))
	assert.Equal(1, CountPermutations(9, 9, []int{1, 2, 3, 4, 5, 6, 7, 8, 9}))
	assert.Equal(29093, CountPermutations(9, 2, []int{1, 2, 3, 4, 5, 6, 7, 9, 8}))
}