package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func newModuloFactorial(n, p uint64) uint64 {
	factorial := uint64(1)
	for i := uint64(1); i <= n; i++ {
		factorial *= i
	}

	x := factorial
	for x%p == 0 {
		x /= p
	}
	return x % p
}



func TestNewModuloFactorial(t *testing.T) {
	// Test cases
	assert.Equal(t, uint64(6), newModuloFactorial(3, 7))
	assert.Equal(t, uint64(1), newModuloFactorial(10, 3))
	assert.Equal(t, uint64(4), newModuloFactorial(11, 7))
	assert.Equal(t, uint64(1), newModuloFactorial(45, 7))
	assert.Equal(t, uint64(2), newModuloFactorial(14, 7))
	assert.Equal(t, uint64(3152), newModuloFactorial(1919, 10007))
	assert.Equal(t, uint64(3679), newModuloFactorial(810, 10007))
	assert.Equal(t, uint64(1), newModuloFactorial(1, 2))
	assert.Equal(t, uint64(10), newModuloFactorial(5, 11))
	assert.Equal(t, uint64(5), newModuloFactorial(6, 13))
	assert.Equal(t, uint64(13), newModuloFactorial(8, 17))
	assert.Equal(t, uint64(16), newModuloFactorial(15, 19))
	assert.Equal(t, uint64(1), newModuloFactorial(21, 23))
	assert.Equal(t, uint64(28), newModuloFactorial(30, 29))
	assert.Equal(t, uint64(100), newModuloFactorial(100, 101))
}