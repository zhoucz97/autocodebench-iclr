package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func EvenSumOrOddProduct(a, b int) int {
    sum := a + b
    if sum%2 == 0 {
        return sum
    } else {
        return a * b
    }
}

func TestEvenSumOrOddProduct(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(6, EvenSumOrOddProduct(2, 3))
	assert.Equal(10, EvenSumOrOddProduct(5, 5))
	assert.Equal(2, EvenSumOrOddProduct(1, 1))
	assert.Equal(0, EvenSumOrOddProduct(0, 0))
	assert.Equal(-2, EvenSumOrOddProduct(-1, -1))
	assert.Equal(300, EvenSumOrOddProduct(100, 200))
	assert.Equal(12, EvenSumOrOddProduct(3, 4))
	assert.Equal(0, EvenSumOrOddProduct(-5, 5))
	assert.Equal(56, EvenSumOrOddProduct(7, 8))
	assert.Equal(90, EvenSumOrOddProduct(9, 10))
	assert.Equal(154, EvenSumOrOddProduct(11, 14))
}