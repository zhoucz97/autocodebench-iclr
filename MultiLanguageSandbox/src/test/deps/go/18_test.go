package main
import (
    "math/big"
    "testing"
    "github.com/stretchr/testify/assert"
)

func countPermutationsOfBinaryString(n, m int) int {
    if m < 0 || m > n {
        return 0
    }
    // To optimize, we choose the smaller of m and n-m to minimize computations
    k := m
    if n-m < m {
        k = n - m
    }
    result := big.NewInt(1)
    for i := 1; i <= k; i++ {
        result.Mul(result, big.NewInt(int64(n-k+i)))
        result.Div(result, big.NewInt(int64(i)))
    }
    return int(result.Int64())
}

func TestCountPermutationsOfBinaryString(t *testing.T) {
	assert := assert.New(t)

	assert.Equal(2, countPermutationsOfBinaryString(2, 0))
	assert.Equal(0, countPermutationsOfBinaryString(2, 1))
	assert.Equal(0, countPermutationsOfBinaryString(3, 0))
	assert.Equal(3, countPermutationsOfBinaryString(3, 1))
	assert.Equal(0, countPermutationsOfBinaryString(3, 2))
	assert.Equal(145422675, countPermutationsOfBinaryString(30, 2))
	assert.Equal(4, countPermutationsOfBinaryString(4, 2))
	assert.Equal(1, countPermutationsOfBinaryString(5, 5))
	assert.Equal(13884156, countPermutationsOfBinaryString(33, 17))
	assert.Equal(1, countPermutationsOfBinaryString(1000000, 1000000))
	// Add more test cases if necessary
}