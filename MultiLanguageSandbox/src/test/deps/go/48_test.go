package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func addDigits(num int) int {
    for num >= 10 {
        sum := 0
        for num > 0 {
            sum += num % 10
            num /= 10
        }
        num = sum
    }
    return num
}

func TestAddDigits(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(2, addDigits(38))
	assert.Equal(0, addDigits(0))
	assert.Equal(9, addDigits(9))
	assert.Equal(6, addDigits(123))
	assert.Equal(6, addDigits(456))
	assert.Equal(9, addDigits(9999))
	assert.Equal(1, addDigits(100))
	assert.Equal(2, addDigits(1010))
	assert.Equal(1, addDigits(1234))
	assert.Equal(3, addDigits(9876))
	assert.Equal(1, addDigits(199))
}