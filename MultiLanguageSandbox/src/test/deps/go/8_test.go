package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func countOddNumbers(count int, nums []int) int {
    oddCount := 0
    for i := 0; i < count && i < len(nums); i++ {
        if nums[i]%2 != 0 {
            oddCount++
        }
    }
    return oddCount
}



func TestCountOddNumbers(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(3, countOddNumbers(5, []int{1, 4, 3, 2, 5}))
	assert.Equal(0, countOddNumbers(4, []int{2, 2, 0, 0}))
	assert.Equal(4, countOddNumbers(6, []int{7, 7, 8, 1, 9, 10})) // Additional Test Sample
}