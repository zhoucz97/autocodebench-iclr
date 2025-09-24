package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func calculateEvenSum(numbers []int) int {
    sum := 0
    for _, num := range numbers {
        if num%2 == 0 {
            sum += num
        }
    }
    return sum
}

func TestCalculateEvenSum(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(6, calculateEvenSum([]int{1, 4, 3, 2, 5}))
	assert.Equal(4, calculateEvenSum([]int{2, 2, 0, 0}))
	assert.Equal(0, calculateEvenSum([]int{7, 11, 19})) // Should return 0 because there are no even numbers
	assert.Equal(12+14+16+18+20, calculateEvenSum([]int{12, 14, 16, 18, 20}))
}