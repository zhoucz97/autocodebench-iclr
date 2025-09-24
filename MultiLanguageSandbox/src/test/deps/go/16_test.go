package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func findLongestConsecutiveOnesLength(n uint64) int {
    maxLen := 0
    currentLen := 0
    
    for n > 0 {
        if n&1 == 1 {
            currentLen++
            if currentLen > maxLen {
                maxLen = currentLen
            }
        } else {
            currentLen = 0
        }
        n >>= 1
    }
    
    return maxLen
}

func TestFindLongestConsecutiveOnesLength(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(3, findLongestConsecutiveOnesLength(7))
	assert.Equal(2, findLongestConsecutiveOnesLength(13))
	assert.Equal(3, findLongestConsecutiveOnesLength(12345))         // New test sample
	assert.Equal(4, findLongestConsecutiveOnesLength(0b11011101111)) // New test sample using binary literal for clarity
	assert.Equal(32, findLongestConsecutiveOnesLength(0xFFFFFFFF))   // New test sample: all ones for a 32-bit number
	assert.Equal(0, findLongestConsecutiveOnesLength(0))             // New test sample: no ones in a zero
}