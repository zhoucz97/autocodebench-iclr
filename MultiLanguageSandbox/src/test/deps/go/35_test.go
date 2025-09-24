package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func GameWinner(n int, piles []int) string {
    if n == 1 {
        return "Dan" // since the only pile is the last one, Charlie has to move it and loses
    }
    
    total := 0
    for i := 0; i < n-1; i++ {
        total += piles[i]
    }
    
    if total%2 == 0 {
        return "Dan"
    } else {
        return "Charlie"
    }
}

func TestGameWinner(t *testing.T) {
	assert := assert.New(t)
	assert.Equal("Dan", GameWinner(3, []int{1, 2, 2}))
	assert.Equal("Charlie", GameWinner(5, []int{5, 5, 5, 5, 5}))
	assert.Equal("Charlie", GameWinner(3, []int{2, 1, 2}))
	assert.Equal("Charlie", GameWinner(4, []int{3, 3, 3, 3}))
	assert.Equal("Charlie", GameWinner(2, []int{1, 1}))
	assert.Equal("Charlie", GameWinner(2, []int{2, 1}))
	assert.Equal("Dan", GameWinner(11, []int{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}))
	assert.Equal("Charlie", GameWinner(10, []int{2, 2, 2, 2, 2, 2, 2, 2, 2, 1}))
	assert.Equal("Charlie", GameWinner(10, []int{10, 10, 10, 10, 10, 10, 10, 10, 10, 10}))
	assert.Equal("Dan", GameWinner(10, []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))
}