package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func extraNumber(a, b, c int) int {
    if a == b {
        return c
    } else if a == c {
        return b
    } else {
        return a
    }
}

func TestExtraNumber(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(7, extraNumber(2, 7, 2), "Extra number should be 7")
	assert.Equal(3, extraNumber(3, 2, 2), "Extra number should be 3")
	assert.Equal(1, extraNumber(5, 5, 1), "Extra number should be 1")
	assert.Equal(3, extraNumber(500000000, 3, 500000000), "Extra number should be 3")
	assert.Equal(3, extraNumber(500000000, 500000000, 3), "Extra number should be 3")
}