package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func sumOfXorSubarrays(A []int) int64 {
    n := len(A)
    total := int64(0)
    
    for i := 0; i < 31; i++ { // Check each bit position from 0 to 30
        mask := 1 << i
        count := 0
        res := 0
        
        for j := 0; j < n; j++ {
            if (A[j] & mask) != 0 {
                count++
            }
            if count%2 == 1 {
                res += (j + 1)
            } else {
                res += (n - j)
            }
        }
        
        total += int64(res) * int64(mask)
    }
    
    return total
}

func TestSumOfXorSubarrays(t *testing.T) {
	assert.Equal(t, int64(39), sumOfXorSubarrays([]int{1, 2, 3, 4, 5}))
	assert.Equal(t, int64(4), sumOfXorSubarrays([]int{1, 1, 1}))
	assert.Equal(t, int64(9), sumOfXorSubarrays([]int{2, 3, 1}))
	assert.Equal(t, int64(74), sumOfXorSubarrays([]int{4, 5, 7, 9}))
	assert.Equal(t, int64(0), sumOfXorSubarrays([]int{0, 0, 0, 0}))
	assert.Equal(t, int64(72), sumOfXorSubarrays([]int{8, 8, 8, 8, 8}))
	assert.Equal(t, int64(125), sumOfXorSubarrays([]int{3, 6, 9, 12, 15}))
	assert.Equal(t, int64(390), sumOfXorSubarrays([]int{10, 20, 30, 40, 50}))
	assert.Equal(t, int64(192), sumOfXorSubarrays([]int{16, 16, 16, 16, 16, 16}))
	assert.Equal(t, int64(192), sumOfXorSubarrays([]int{1, 3, 5, 7, 9, 11, 13}))
	assert.Equal(t, int64(218), sumOfXorSubarrays([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}))
}