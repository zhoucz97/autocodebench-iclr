package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func hammingDistance(x int, y int) int {
    xor := x ^ y
    distance := 0
    for xor != 0 {
        distance += xor & 1
        xor >>= 1
    }
    return distance
}

func TestHammingDistance(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(2, hammingDistance(1, 2), "1 and 2 should have a Hamming distance of 2")
	assert.Equal(2, hammingDistance(4, 7), "4 and 7 should have a Hamming distance of 2")
	assert.Equal(3, hammingDistance(25, 30), "25 and 30 should have a Hamming distance of 3")
	assert.Equal(0, hammingDistance(0, 0), "0 and 0 should have a Hamming distance of 0")
	assert.Equal(28, hammingDistance(0xFFFFFFF, 0), "0xFFFFFFF and 0 should have a Hamming distance of 28")
}