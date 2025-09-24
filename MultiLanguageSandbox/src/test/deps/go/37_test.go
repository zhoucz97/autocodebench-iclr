package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func CountHandshakes(n int, order []int) int64 {
    var total int64 = 0
    present := make(map[int]bool)
    
    for _, id := range order {
        count := 0
        for existingID := range present {
            if existingID < id {
                count++
            }
        }
        total += int64(count)
        present[id] = true
    }
    
    return total
}

func TestCountHandshakes(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(int64(2), CountHandshakes(4, []int{2, 1, 3, 0}))
	assert.Equal(int64(15), CountHandshakes(6, []int{0, 1, 2, 3, 4, 5}))
	assert.Equal(int64(1), CountHandshakes(3, []int{1, 2, 0}))
	assert.Equal(int64(0), CountHandshakes(4, []int{3, 2, 1, 0}))
	assert.Equal(int64(6), CountHandshakes(4, []int{0, 1, 2, 3}))
	assert.Equal(int64(0), CountHandshakes(6, []int{5, 4, 3, 2, 1, 0}))
	assert.Equal(int64(5), CountHandshakes(4, []int{0, 2, 1, 3}))
	assert.Equal(int64(3), CountHandshakes(5, []int{3, 1, 4, 2, 0}))
	assert.Equal(int64(4), CountHandshakes(4, []int{1, 0, 3, 2}))
	assert.Equal(int64(1), CountHandshakes(3, []int{2, 0, 1}))
	assert.Equal(int64(7), CountHandshakes(5, []int{1, 3, 0, 2, 4}))
	assert.Equal(int64(0), CountHandshakes(5, []int{4, 3, 2, 1, 0}))
}

// To run tests in Go, you can use the following command:
// go test -v