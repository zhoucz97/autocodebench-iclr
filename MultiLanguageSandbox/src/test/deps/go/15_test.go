package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func CountValidCoinTossSequences(n int) uint64 {
    if n == 0 {
        return 0
    }
    if n == 1 {
        return 2
    }
    if n == 2 {
        return 3
    }
    
    a, b := uint64(2), uint64(3)
    for i := 3; i <= n; i++ {
        c := a + b
        a = b
        b = c
    }
    return b
}

func TestCountValidCoinTossSequences(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(uint64(2), CountValidCoinTossSequences(1))
	assert.Equal(uint64(3), CountValidCoinTossSequences(2))
	assert.Equal(uint64(5), CountValidCoinTossSequences(3))
	assert.Equal(uint64(8), CountValidCoinTossSequences(4))  // Additional test
	assert.Equal(uint64(13), CountValidCoinTossSequences(5)) // Additional test
	// Feel free to add more tests here
	assert.Equal(uint64(267914296), CountValidCoinTossSequences(40)) // Additional test
	assert.Equal(uint64(165580141), CountValidCoinTossSequences(39))
	assert.Equal(uint64(102334155), CountValidCoinTossSequences(38))
	// Use t.Log instead of fmt.Print or fmt.Println in tests
	t.Log("All tests passed!")
}