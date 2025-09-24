package main
import (
    "math"
    "testing"
    "github.com/stretchr/testify/assert"
)

func findIntegers(c int, d int) int {
	D := float64(c*c - 4*d)
	if D < 0 {
		return -1
	}
	s := math.Sqrt(D)
	if s != float64(int(s)) {
		return -1
	}
	// Check if (c - s) and (c + s) are even, so division by 2 gives integer
	if (c-int(s))%2 != 0 || (c+int(s))%2 != 0 {
		return -1
	}
	a := (c - int(s)) / 2
	b := (c + int(s)) / 2
	if a > b {
		a, b = b, a
	}
	return a
}

func TestFindIntegers(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(2, findIntegers(5, 6))
	assert.Equal(3, findIntegers(6, 9))
	assert.Equal(3, findIntegers(7, 12))
	assert.Equal(-1, findIntegers(7, 11))
	assert.Equal(1, findIntegers(9, 8))
	assert.Equal(5, findIntegers(10, 25))
	assert.Equal(-1, findIntegers(10000, 8765))
}