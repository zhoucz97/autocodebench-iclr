package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func sumOfMultiples(n int) int {
    sum := 0
    for i := 1; i <= n; i++ {
        if i%3 == 0 || i%5 == 0 {
            sum += i
        }
    }
    return sum
}

func TestSumOfMultiples(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(33, sumOfMultiples(10))
	assert.Equal(60, sumOfMultiples(15))
	assert.Equal(98, sumOfMultiples(20))
	assert.Equal(8, sumOfMultiples(5))
	assert.Equal(3, sumOfMultiples(3))
	assert.Equal(14, sumOfMultiples(6))
	assert.Equal(23, sumOfMultiples(9))
	assert.Equal(45, sumOfMultiples(12))
	assert.Equal(60, sumOfMultiples(17))
	assert.Equal(119, sumOfMultiples(21))
	assert.Equal(168, sumOfMultiples(25))
}